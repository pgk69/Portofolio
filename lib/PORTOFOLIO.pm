package PORTOFOLIO;

#-------------------------------------------------------------------------------
# Letzte Aenderung:     $Date: 2012-06-04 15:53:45 +0200 (Mo, 04 Jun 2012) $
#                       $Revision: 853 $
#                       $Author: xck90n1 $
#
# Aufgabe:				 - Ausfuehrbarer Code von programm.pl
#
# $Id: PROGRAMM.pm 853 2012-06-04 13:53:45Z xck90n1 $
# $URL: https://svn.fiducia.de/svn/multicom/trunk/multicom/Framework%20OO/lib/PROGRAMM.pm $
#-------------------------------------------------------------------------------

use utf8;      # so literals and identifiers can be in UTF-8
# use strict;    # quote strings, declare variables
# use warnings;  # on by default
# use warnings  qw(FATAL utf8);    # fatalize encoding glitches
use v5.16;     # or later to get "unicode_strings" feature
no warnings 'redefine';

# use charnames qw(:full :short);  # unneeded in v5.16
# binmode STDOUT, ":utf8";

use vars qw($VERSION $SVN $OVERSION);

use constant SVN_ID => '($Id: PROGRAMM.pm 853 2012-06-04 13:53:45Z xck90n1 $)

$Author: xck90n1 $

$Revision: 853 $
$Date: 2012-06-04 15:53:45 +0200 (Mo, 04 Jun 2012) $

$URL: https://svn.fiducia.de/svn/multicom/trunk/multicom/Framework%20OO/lib/PROGRAMM.pm $

';

($VERSION = SVN_ID) =~ s/^(.*\$Revision: )([0-9]*)(.*)$/1.0 R$2/ms;
$SVN      = $VERSION . ' ' . SVN_ID;
$OVERSION = $VERSION;

use base 'Exporter';

our @EXPORT    = ();
our @EXPORT_OK = ();

use vars @EXPORT, @EXPORT_OK;

use vars qw(@ISA);
@ISA = qw(DBAccess Trace Configuration CmdLine);

use Trace;
use CmdLine;
use Configuration;
use DBAccess;

#
# Module
#
use Data::Dumper;
use Utils;
use LockFile::Simple;
use Scalar::Util qw(looks_like_number);
use LWP::Simple;
use Date::Parse;
use Date::Format;
use POSIX();
use Term::ANSIColor qw(color RESET :constants :constants256);
use File::Copy;
use File::Path;
use File::Basename;
use FindBin qw($Bin $Script $RealBin $RealScript);
use HTML::Tree;
use Cpanel::JSON::XS qw(encode_json decode_json);
use Data::Clone;
use Storable;
use Finance::Quote;
use Locale::Currency;
use Time::HiRes qw(sleep);
use Selenium::Chrome;
use Selenium::Remote::WDKeys 'KEYS';
use Selenium::ActionChains;

#use Test::More "no_plan";

#
# Konstantendefinition
#

#
# Variablendefinition
#
my %HighLow;
my %reset;
my %umlaute = ("ä" => "ae", "Ä" => "Ae", "ü" => "ue", "Ü" => "Ue", "ö" => "oe", "Ö" => "Oe", "ß" => "ss" );
my $umlautkeys = join ("|", keys(%umlaute));

#
# Methodendefinition
#
sub version {
  my $self     = shift();
  my $pversion = shift();

  $OVERSION =~ m/^([^\s]*)\sR([0-9]*)$/;
  my ($oVer, $oRel) = ($1, $2);
  $oVer = 1 if (!$oVer);
  $oRel = 0 if (!$oRel);

  if (defined($pversion)) {
    $pversion =~ m/^([^\s]*)\sR([0-9]*)$/;
    my ($pVer, $pRel) = ($1, $2);
    $pVer = 1 if (!$pVer);
    $pRel = 0 if (!$pRel);
    $VERSION = $oRel gt $pRel ? "$pVer R$oRel" : "$pVer R$pRel";
  }

  return wantarray() ? ($VERSION, $OVERSION) : $VERSION;
} ## end sub version


sub new {
  #################################################################
  #     Legt ein neues Objekt an
  my $self  = shift;
  my $class = ref($self) || $self;
  my @args  = @_;

  my $ptr = {};
  bless $ptr, $class;
  $ptr->_init(@args);

  return $ptr;
} ## end sub new


sub _init {
  #################################################################
  #   Initialisiert ein neues Objekt
  my $self = shift;
  my @args = @_;

  no autovivification;

  $self->{Startzeit} = time();

  $VERSION = $self->version(shift(@args));

  Trace->Trc('S', 1, 0x00001, Configuration->prg, $VERSION . " (" . $$ . ")" . " Test: " . Trace->test() . " Parameter: " . CmdLine->new()->{ArgStrgRAW});

  if (Configuration->config('Prg', 'Plugin')) {
    # refs ausschalten wg. dyn. Proceduren
    no strict 'refs';
    my %plugin = ();

    # Bearbeiten aller Erweiterungsmodule die in der INI-Datei
    # in Sektion [Prg] unter "Plugin =" definiert sind
    foreach (split(' ', Configuration->config('Prg', 'Plugin'))) {

      # Falls ein Modul existiert
      if (-e "$self->{Pfad}/plugins/${_}.pm") {

        # Einbinden des Moduls
        require $_ . '.pm';
        $_->import();

        # Initialisieren des Moduls, falls es eine eigene Sektion
        # [<Modulname>] fuer das Module in der INI-Datei gibt
        $plugin{$_} = eval {$_->new(Configuration->config('Plugin ' . $_))};
        eval {$plugin{$_} ? $plugin{$_}->DESTROY : ($_ . '::DESTROY')->() if (CmdLine->option('erase'));};
      } ## end if (-e "$self->{Pfad}/plugins/${_}.pm")
    } ## end foreach (split(' ', Configuration...))
    use strict;
  } ## end if (Configuration->config...)
  # Module::Refresh->refresh;

  # Test der benoetigten INI-Variablen
  # DB-Zugriff

  # Web-Abfrage
  Trace->Exit(1, 0, 0x0f009, 'Service') if (!defined(Configuration->config('Prg', 'Service')));
  Trace->Exit(1, 0, 0x0f009, 'Flags')   if !defined(Configuration->config('Flags'));
  my @services = split(/ /, Configuration->config('Prg', 'Service'));
  foreach my $service (@services) {
    Trace->Exit(1, 0, 0x0f009, 'Stockservice '.$service, 'URL') if !defined(Configuration->config('Stockservice '.$service, 'URL'));
  }
  $self->{Services} = \@services;
  $self->{Retry} = Configuration->config('Prg', 'Retry') || 1;

  # Ergebnisausgabe und Sicherung
  if (Configuration->config('Prg', 'LockFile')) {
    $self->{LockFile} = File::Spec->canonpath(Utils::extendString(Configuration->config('Prg', 'LockFile'), "BIN|$Bin|SCRIPT|" . uc($Script)));
    $self->{Lock} = LockFile::Simple->make(-max       => 5,
                                           -delay     => 1,
                                           -format    => '%f',
                                           -autoclean => 1,
                                           -stale     => 1);
    my $errtxt;
    $SIG{'__WARN__'} = sub {$errtxt = $_[0]};
    my $lockerg = $self->{Lock}->trylock($self->{LockFile});
    undef($SIG{'__WARN__'});
    if (defined($errtxt)) {
      $errtxt =~ s/^(.*) .+ .+ line [0-9]+.*$/$1/;
      chomp($errtxt);
      Trace->Trc('S', 1, 0x02000, $errtxt) if defined($errtxt);
    }
    if (!$lockerg) {
      Trace->Exit(0, 1, 0x0a000, Configuration->prg, $self->{LockFile});
    } else {
      Trace->Trc('S', 1, 0x02001, $self->{LockFile});
    }
  } ## end if (Configuration->config...)
  $self->{AutoCommit} = Configuration->config('DB', 'AUTOCOMMIT') || 0;

  # Rücksichern der Variablenwerte
  if (Configuration->config('Prg', 'VarFile') && !CmdLine->option('flush')) {
    $self->{VarFile} = Utils::extendString(Configuration->config('Prg', 'VarFile'), "BIN|$Bin|SCRIPT|" . uc($Script));
    if (exists($self->{VarFile})) {
      eval {$self->{Kurs} = retrieve($self->{VarFile});};
    }
  }

  # HighLow-Werte sichern
  if (Configuration->config('Prg', 'HighLow')) {
    my $hlFile = Utils::extendString(Configuration->config('Prg', 'HighLow'), "BIN|$Bin|SCRIPT|" . uc($Script));
    if (-r $hlFile) {
      my $HighLowRef = retrieve($hlFile);
      %HighLow = %$HighLowRef;
    }
    if (my $reset = CmdLine->option('Reset')) {
      foreach my $val (split(" ", $reset)) {
        Trace->Trc('I', 2, 0x02003, $val);
        $reset{$val}   = 1;
        undef($HighLow{$val}) if $val =~ /^L_/;
        undef($HighLow{$val}) if $val =~ /^H_/;
      }
    }
  }

  # BasisWaehrung festlegen
  $self->{BasisCur} = Configuration->config('Prg', 'BasisCurrency') || 'EUR';
  if (!defined(code2currency($self->{BasisCur}))) {
    Trace->Trc('I', 2, 0x02002, $self->{BasisCur});
    $self->{BasisCur} = 'EUR';
  }

  # Positionen aufsummieren
  $self->{SumPos} = Configuration->config('Prg', 'PosAufsummieren') ? 1 : 0;

  # Sleeptimer setzen
  $self->{Sleeptimer} = CmdLine->option('Sleep') || 0;

  # Gesamtliste erzeugen
  $self->{Gesamtliste} = Configuration->config('Prg', 'Gesamtliste') || undef;

  # Zeitzone festlegen
  my ($S, $M, $H, $d, $m, $y, $wd, $yd, $sz) = localtime(time);
  $m  += 1;
  $yd += 1;
  $y  += 1900;
  $self->{TZ} = 'EST';
  my $dlsstart = (6 * $y - int($y / 4) - 2) % 7 + 8;
  my $dlsend = (7 - ($y * 5 / 4 + 1) % 7);
  if (   (($m > 3) && ($m < 11))
      || (($m == 3)  && ($d >= $dlsstart))
      || (($m == 11) && ($d < $dlsend))) {
    $self->{TZ} = 'EDT';
  }

  if (my $Testkurs = Configuration->config('Stockservice', 'Testkurs')) {
    $self->{Testkurs} = $Testkurs;
  }

  # Maximales Alter von Kursinfos
  $self->{MaxDelay} = Configuration->config('Stockservice', 'MaxDelay') || 86400;

  # Differenz zu UTC
  $self->{UTCDelta} = Configuration->config('Stockservice', 'UTCDelta') || +0100;

  # Farben
  $Term::ANSIColor::AUTORESET = 1;
  $self->{Color}->{normal}  = ' ' . (Configuration->config('Color', 'normal')  || 'on_ansi0');
  $self->{Color}->{invers}  = ' ' . (Configuration->config('Color', 'invers')  || 'on_grey5');
  $self->{Color}->{clear}   =        Configuration->config('Color', 'clear')   || 'clear';
  $self->{Color}->{neutral} =       (Configuration->config('Color', 'neutral') || 'white') . ' ';
  $self->{Color}->{pp}      =       (Configuration->config('Color', 'pp')      || 'green') . ' ';
  $self->{Color}->{p}       =       (Configuration->config('Color', 'p')       || 'cyan') . ' ';
  $self->{Color}->{m}       =       (Configuration->config('Color', 'm')       || 'rgb525') . ' ';
  $self->{Color}->{mm}      =       (Configuration->config('Color', 'mm')      || 'rgb501') . ' ';
  $self->{Color}->{reset}   =        Configuration->config('Color', 'reset')   || ($self->{Color}->{neutral} . $self->{Color}->{normal});

} ## end sub _init


sub DESTROY {
  #################################################################
  #     Zerstoert das Objekt an
  my $self = shift;
  my ($rc, $sig) = (0, 0);
  $rc  = ($? >> 8);
  $sig = $? & 127;
  if ($@ || $rc != 0 || $sig != 0) {
    my ($routine, $i) = ((caller(0))[3] . ':', 0);
    while (defined(caller(++$i))) {
      $routine .=
        (caller($i))[3] . '(' . (caller($i - 1))[2] . '):';
    }
    Trace->Trc('S', 1, 0x00007, "$routine $@ $! $?");
    Trace->Log('Log', 0x10013, $@, $!, $?);
  }
  for my $parent (@ISA) {
    if (my $coderef = $self->can($parent . '::DESTROY')) {
      $self->$coderef();
    }
  }

  # Eigentlich nicht noetig, da -autoclean => 1
  if ($self->{Lock}) {
    $self->{Lock}->unlock($self->{LockFile});
  }
} ## end sub DESTROY


sub wait_for_page_to_load {
  my $self = shift;

  my $sel = shift;

  my $ret         = 0;
  my $sleeptime   = 10;  # milliseconds
  my $readyState  = "";
  my $jqueryState = '';

  while (($readyState ne 'complete') and ($jqueryState ne 0)) {
    $ret++;
    $readyState = $sel->execute_script("return document.readyState");
    $jqueryState = $sel->execute_script("return jQuery.active");
    # print "'".$jqueryState."' is the result of return jQuery.active\n";
    # print "'".$readyState."' is the result of return document.readyState\n";
  }
  return $ret;
}


sub _webabruf_comdirect {
  #################################################################
  # Durchfuehren eines Webabrufs

  #
  # Prozedurnummer 1
  #
  my $self = shift;

  my $symbols = shift;
  my $try = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 3, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;

  my %name;
  foreach my $symbol (@$symbols) {
    my $kursptr = $self->{Kurs}->{$symbol};
    if (defined($kursptr->{Name})) {
      $name{$kursptr->{Name}} = $symbol;
    } else {
      $name{$symbol} = $symbol;
    }
  }

  my $firstsymbolwait = 3;

  # my $adblockfile = '/Users/pgk/Documents/00_Entwicklung/Finance/Portofolio/plugins/adblock_plus-3.0.4-an+fx.xpi';

  my $URL = Configuration->config('Stockservice comdirect', 'URL');
  my @args = ('window-size=1260,960', 'disable-gpu', 'no-sandbox');
  push(@args, 'headless') if Configuration->config('Stockservice comdirect', 'Headless');;
  #push(@args, 'incognito');
  Trace->Trc('I', 2, "Instantiiere Web Loader");
  my $sel = Selenium::Chrome->new('browser_name'       => 'chrome',
                                  'platform'           => 'MAC',
                                  'binary'             => Configuration->config('Stockservice comdirect', 'ChromeDriver'),
                                  'binary_port'        => 9515,
                                   # 'custom_args'        => '--verbose --log-path=/tmp/chromedriver.log',
                                  'startup_timeout'    => 60,
                                  'extra_capabilities' => {'chromeOptions' => {'binary' => Configuration->config('Stockservice comdirect', 'Chrome'),
                                                                               'args'   => \@args
                                                                               }
                                                           }
                                  );
  
  $sel->set_timeout('page load', 100000);
  Trace->Trc('I', 2, "Lade $URL");
  $sel->get($URL);
  $self->wait_for_page_to_load($sel);
  
  Trace->Trc('I', 2, "Parse $URL");
  # Get rid of banner
  my $field;
  eval {undef $field; $field = $sel->find_element('closeCookieBanner', 'id');};
  if ($field) {
    $sel->mouse_move_to_location(element => $field);
    sleep(1);
    $field->click();
  }
      
  my %symbolhash = map {$_ => 1} @$symbols;

  my $title = 'Wertpapiersuche und Kursabfrage';
  # Fuer alle Symbole
  my @loop = sort(keys(%name));
  my $exchange_added = 0;
  SYMBOL: foreach my $name (@loop) {
    my $symbol = $name{$name};
    my $puffer;
    my $kursptr = $self->{Kurs}->{$symbol};
    my $search = $kursptr->{ISIN};
    $search = $kursptr->{WKN} if !$search;
    $search = $kursptr->{Symbol} if !$search;
    $search = $kursptr->{Name} if !$search;
    $search = $symbol if !$search && $kursptr->{exchange};
    next SYMBOL if !$search;

    Trace->Trc('I', 5, "Suche nach ${search}");
    # Set the search ISIN
    eval {undef $field; $field = $sel->find_element('SEARCH_VALUE', 'name');};
    my $versuch = 1 + $self->{Retry} - $try;

    # Trace->Trc('I', 2, 0x02104, "$name/$search/$try");
    $field->clear();
    $field->send_keys("$search", KEYS->{'enter'});
    sleep($self->{Sleeptimer}) if ($self->{Sleeptimer});
    $self->wait_for_page_to_load($sel);
    if ($self->{Sleeptimer}) {
      sleep($self->{Sleeptimer});
      $field->send_keys(KEYS->{'enter'});
      $self->wait_for_page_to_load($sel);
    }

    my $TIMEOUT = 2;
    my $MAXTRY  = 20;
    my $DELAY   = 0.1;
    my $sideloaded = 0;
    my $name = "";

    while (!$sideloaded && $MAXTRY) {
      eval{undef $field; $field = $sel->find_element('h1[class^=headline]', 'css');};
      if ($field) {
        eval{undef $name; $name = $field->get_text()};
        $sideloaded = $name && ($name !~ m/cominvest/) && ($name ne $title) && ($name ne 'Wertpapiersuche und Kursabfrage');
      }
      if (!$sideloaded) {
        $MAXTRY--;
        sleep($DELAY);
      }
    }
    next SYMBOL if $name =~ /cominvest/;
    if (!$sideloaded) {
      Trace->Trc('I', 2, "${search} nicht gefunden");
      next SYMBOL;
    }

    # Lang & Schwarz umbenennen
    $name =~ s/^Lang.*?([^\s]*)\s[^\s]*$/L\&S\-$1/;

    # Vontobel Financial Products umbenennen
    $name =~ s/^Vontobel\sFinancial\sProducts\s(.*)$/Vontobel\-$1/;
    
    # Deutsche Bank AG Produkte umbenennen
    $name =~ s/^Deutsche\sBank/DeuBa/i;
    
    # Umlaute entfernen
    $name =~ s/($umlautkeys)/$umlaute{$1}/g;
    Trace->Trc('I', 2, 0x02107, "${search}/${name}");

    $title = $name;
    next SYMBOL if ($kursptr->{RegName} && ($name !~ /$kursptr->{RegName}/));

    sleep($self->{Sleeptimer});

    if ($name =~ /(.*)?\s(.+)$/) {
      $name = $1;
      $puffer->{Typ} = $2;
    }
    $puffer->{Name} = $name;
    
    # get all the table contents of the site
    my @labels = $sel->find_elements('simple-table__cell', 'class');
    while (my $field = shift(@labels)) {
      # filter keys without value
      next if $field->get_attribute('colspan');
    
      my $value = shift(@labels);
      # filter unwanted keys
      my $fieldtext = $field->get_text();
      next if !$fieldtext;
      next if $fieldtext =~ /^Gestern/;
      next if $fieldtext =~ /^Heute/;
      next if $fieldtext =~ /^Demoversion/;
      next if $fieldtext =~ /^Zielmarktkriterien/;
      next if $fieldtext =~ /^[0-9]{2}\.[0-9]{2}\.[0-9]{2}/;
      my $valuetext = $value ? $value->get_text() : '--';
      next if $valuetext =~ /^[\-\s]+$/;
      $fieldtext =~ s/^\s+|\s+$//g;
      $valuetext =~ s/^\s+|\s+$//g;
      if ($valuetext =~ /^[\-\+]?[0-9\.]+\,/) {
        $valuetext =~ s/\.//g;
        $valuetext =~ s/\,/\./g;
      }
      $puffer->{$fieldtext} = $valuetext;
    }
    
    # Branche
    $puffer->{Sector} = $puffer->{Branche} || $puffer->{Anlagekategorie} || $puffer->{Fondskategorie} || 'Sonstige Branche';
    delete($puffer->{Branche});
    # character adjustment
    foreach ('Sector', 'Name', 'Basiswert') {
      next if !$puffer->{$_};
      $puffer->{$_} =~ s/Ä/Ae/g;
      $puffer->{$_} =~ s/ä/ae/g;
      $puffer->{$_} =~ s/Ö/Oe/g;
      $puffer->{$_} =~ s/ö/oe/g;
      $puffer->{$_} =~ s/Ü/Ue/g;
      $puffer->{$_} =~ s/ü/ue/g;
      $puffer->{$_} =~ s/ß/ss/g;
    }

    # Price & Currency
    my $Price_cur = $puffer->{Aktuell} || $puffer->{Geld} || $puffer->{'Rücknahmepreis'} || '0.00';
    if ($Price_cur =~ /^([0-9]+\.[0-9]{2}).*?([A-Za-z]*)$/s) {
      $puffer->{Price}    = $1;
      $puffer->{Currency} = $2;

      if (!$puffer->{Currency}) {
        $puffer->{Currency} = $puffer->{'Währung'} if $puffer->{'Währung'};
        $puffer->{Currency} = $2 if $puffer->{Nennwert} && ($puffer->{Nennwert} =~ /^([0-9\.]*[0-9,]+).*?([A-Za-z]*)$/s);
      }

      if ($puffer->{Currency}) {
        $puffer->{Currency} = substr($puffer->{Currency}, 0, 3);
        if ($puffer->{Currency} ne $self->{BasisCur}) {
          foreach my $cur ($puffer->{Currency}, $self->{BasisCur}) {
            my ($exchange, $regexchange);
            if ($cur eq $self->{BasisCur}) {
              $exchange    = $puffer->{Currency} . $self->{BasisCur};
              $regexchange = $puffer->{Currency} . '?' . $self->{BasisCur};
            } else {
              $exchange    = $self->{BasisCur} . $puffer->{Currency};
              $regexchange = $self->{BasisCur} . '?' . $puffer->{Currency};
            }
            if ($exchange =~ /^[0-9A-Z]{6}$/) {
              if (!$symbolhash{$exchange}) {
                Trace->Trc('I', 2, "Lege neues Waehrungspaar an $exchange ($puffer->{Name})");
                $self->{Kurs}->{$exchange}->{Name} = $exchange;
                $self->{Kurs}->{$exchange}->{Symbol} = $exchange;
                $self->{Kurs}->{$exchange}->{RegName} = $regexchange;
                $self->{Kurs}->{$exchange}->{Currency} = $puffer->{Currency};
                $self->{Kurs}->{$exchange}->{aktuell} = 0;
                $self->{Kurs}->{$exchange}->{exchange} = 1;
                $symbolhash{$exchange} = 1;
                push(@$symbols, $exchange);
                push(@loop, $exchange) if !defined($name{$exchange});
                $name{$exchange} = $exchange;
                $exchange_added = 1;
              }
            }
          }
        }
      }
    }

    # daily change percent & absolute
    if (!$puffer->{'Diff. Vortag'}) {
      $puffer->{'Diff. Vortag'} = 0;
      my $vortag = $puffer->{'Schluss Vortag'} || $puffer->{Vortag};
      if ($vortag) {
        $puffer->{Vortag} = $vortag;
        $puffer->{'Diff. Vortag'} = 100*($puffer->{Price}-$vortag)/$vortag;
      }
    }

    if ($puffer->{'Diff. Vortag'} && ($puffer->{'Diff. Vortag'} =~ /^([\+\-]?[0-9]+\.[0-9]{2})/)) {
      $puffer->{Change_Day_Percent} = $1;
      if ($puffer->{Vortag} && $puffer->{Price}) {
        $puffer->{Change_Day} = $puffer->{Price} - $puffer->{Vortag};
      } else {
        $puffer->{Change_Day} = $puffer->{Price} * $puffer->{Change_Day_Percent}/(100+$puffer->{Change_Day_Percent});
      }
    }

    # last trade date & time
    my $Datetext = $puffer->{Zeit};

    if ($Datetext =~ /^([0-9\.]{8})[\s]*(.*)$/) {
      $puffer->{Last_Trade_Date} = $1;
      $puffer->{Last_Trade_Time} = $2;
      $puffer->{Last_Trade_Time} = '00:00' if $puffer->{Last_Trade_Time} !~ /^[0-9\:]{5}$/; 
      $puffer->{Last_Trade}      = "$puffer->{Last_Trade_Date} $puffer->{Last_Trade_Time}";
      my ($day, $month, $year) = split(/\./, $puffer->{Last_Trade_Date});
      $puffer->{Last_Trade_TS}   = str2time("$year-$month-$day") || 0;
    }

    # KGV
    $puffer->{KGV} = '';
    if ($puffer->{'KGVe:'}) {
      $puffer->{KGV} = $puffer->{'KGVe:'};
    }
    
    # dividend
    $puffer->{DIV} = '';
    if ($puffer->{'DIVe:'}) {
      $puffer->{DIV}   = $puffer->{'DIVe:'};
      my $Dividendtext = $puffer->{'DIV'};

      if ($Dividendtext =~ /^\+?([0-9]+\.?[0-9]*)/) {
        $puffer->{Dividend_Yield} = $1;
        $puffer->{Dividend} = $puffer->{Dividend_Yield} * $puffer->{Price} / 100;
        $puffer->{Dividend_Currency} = $puffer->{Currency};
      }
    }
    
    $puffer->{Marktkapitalisierung} = $puffer->{'Marktkapital.'} || $puffer->{'Fondsvolumen'} || '';
    $puffer->{Marktkapitalisierung} = '' if $puffer->{Marktkapitalisierung} =~ /^\-\-/;
    if ($puffer->{Marktkapitalisierung} =~ /^([0-9]+\.?[0-9]*)\s([A-Za-z\.]*)\s([A-Za-z\.]*)$/) {
      $puffer->{Marktkapitalisierung} = $1;
      if ($2 eq 'Mrd.') {
        $puffer->{Marktkapitalisierung} *= 1000;
      }  
    }  

    $puffer->{Bruttorendite}        = $puffer->{'Bruttorendite:'} || '';
    $puffer->{Eigenkapitalquote}    = $puffer->{'Eigenkapitalquote:'} || '';
    $puffer->{Umsatz}               = $puffer->{'Umsatz:'} || '';

    # ----------------------

    if (!defined($kursptr->{Last_Trade_TS}) || ($puffer->{Last_Trade_TS} > $kursptr->{Last_Trade_TS})) {
      $kursptr->{aktuell} = 0;

      if ($puffer->{Last_Trade_TS}) {
        foreach (keys(%$puffer)) {
          $kursptr->{$_} = $puffer->{$_};
        }

        $kursptr->{Last_Trade_Days_ago} = int(abs((str2time(time2str('%x', time())) - $puffer->{Last_Trade_TS}) / 86400));
        $kursptr->{aktuell} = 1;
        $kursptr->{letzter_Abruf} = str2time(gmtime());
      } else {
        $kursptr->{Last_Trade} = 0
      }

      if ($kursptr->{Name}) {$kursptr->{Name} =~ s/\s+N$//}
      if (defined($kursptr->{Dividend})) {$kursptr->{Dividend} ||= 0}
      if (!defined($kursptr->{Symbol}) || ($kursptr->{Symbol} eq '')) {$kursptr->{Symbol} = $symbol}

      # Kursinfos speichern
      if (exists($self->{VarFile})) {
        store $self->{Kurs}, $self->{VarFile}
      }
    }
    if ($exchange_added && $kursptr->{exchange}) {
      $self->Reverse_Kurs;
      $exchange_added = 0;
    }
  }

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return 0;
} ## end sub _webabruf_comdirect


sub _webabruf_iExtrading {
  #################################################################
  # Durchfuehren eines Webabrufs
  my $self = shift;

  my $symbols = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 3, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;

  my ($symbolsstr, $filter, %nameMap);
  # Alle Filterattribute sammeln
  my $flags = $self->{Flags};
  while (my ($flagName, $flag) = each %{$flags}) {
    next if !exists($flag->{iExtrading}->{Name});
    $nameMap{$flag->{iExtrading}->{Name}} = $flagName;
    $filter .= "$flag->{iExtrading}->{Name},";
  }
  chop($filter) if defined($filter);

  my $count = Configuration->config('Stockservice iExtrading', 'MaxSymbol') || 100;
  # TODO: MaxSymbol auswerten
  # Alle Symbole sammeln
  my @symbolsfiltered;
  foreach (@{$symbols}) {
    next if /\./;  # Service only available for american symbols
    push(@symbolsfiltered, $_);
  }

  $symbolsstr = join(',', @symbolsfiltered);

  # Fuer alle Symbole
  my ($webAbruf, $stockdata, $infojson);
  if ($symbolsstr) {
    Trace->Trc('I', 2, 0x02104, $symbolsstr);
    # Webaufruf durchfuehren und Ergebnis auswerten
    $webAbruf = Utils::extendString(Configuration->config('Stockservice iExtrading', 'URL'), 'SYMBOL|' . $symbolsstr . '|FILTER|' . $filter);
    Trace->Trc('I', 2, 0x02106, $webAbruf);
    $stockdata = get($webAbruf);
#    $stockdata = '{"MO":{"quote":{"latestUpdate":1511308800000,"primaryExchange":"New York Stock Exchange","change":0,"companyName":"Altria Group Inc.","symbol":"MO","changePercent":0,"latestTime":"November 22, 2017","latestPrice":65.49,"sector":"Consumer Defensive"},"dividends":[{"exDate":"2017-09-14","amount":0.66},{"exDate":"2017-06-13","amount":0.61},{"exDate":"2017-03-13","amount":0.61},{"exDate":"2016-12-20","amount":0.61}]},"T":{"quote":{"latestUpdate":1511308800000,"primaryExchange":"New York Stock Exchange","change":0,"companyName":"AT&T Inc.","symbol":"T","changePercent":0,"latestTime":"November 22, 2017","latestPrice":34.87,"sector":"Communication Services"},"dividends":[{"exDate":"2017-10-06","amount":0.49},{"exDate":"2017-07-06","amount":0.49},{"exDate":"2017-04-06","amount":0.49},{"exDate":"2017-01-06","amount":0.49}]},"IBM":{"quote":{"latestUpdate":1511308800000,"primaryExchange":"New York Stock Exchange","change":0,"companyName":"International Business Machines Corporation","symbol":"IBM","changePercent":0,"latestTime":"November 22, 2017","latestPrice":151.77,"sector":"Technology"},"dividends":[{"exDate":"2017-11-09","amount":1.5},{"exDate":"2017-08-08","amount":1.5},{"exDate":"2017-05-08","amount":1.5},{"exDate":"2017-02-08","amount":1.4}]},"C":{"quote":{"latestUpdate":1511308800000,"primaryExchange":"New York Stock Exchange","change":0,"companyName":"Citigroup Inc.","symbol":"C","changePercent":0,"latestTime":"November 22, 2017","latestPrice":72.26,"sector":"Financial Services"},"dividends":[{"exDate":"2017-11-03","amount":0.32},{"exDate":"2017-08-03","amount":0.32},{"exDate":"2017-04-27","amount":0.16},{"exDate":"2017-02-02","amount":0.16}]},"AGNC":{"quote":{"latestUpdate":1511308800000,"primaryExchange":"Nasdaq Global Select","change":0,"companyName":"AGNC Investment Corp.","symbol":"AGNC","changePercent":0,"latestTime":"November 22, 2017","latestPrice":20.42,"sector":"Real Estate"},"dividends":[{"exDate":"2017-10-30","amount":0.18},{"exDate":"2017-09-28","amount":0.18},{"exDate":"2017-08-29","amount":0.18},{"exDate":"2017-07-27","amount":0.18},{"exDate":"2017-06-28","amount":0.18},{"exDate":"2017-05-26","amount":0.18},{"exDate":"2017-04-26","amount":0.18},{"exDate":"2017-03-29","amount":0.18},{"exDate":"2017-02-24","amount":0.18},{"exDate":"2017-01-27","amount":0.18},{"exDate":"2016-12-28","amount":0.18},{"exDate":"2016-11-28","amount":0.18}]},"K":{"quote":{"latestUpdate":1511308800000,"primaryExchange":"New York Stock Exchange","change":0,"companyName":"Kellogg Company","symbol":"K","changePercent":0,"latestTime":"November 22, 2017","latestPrice":64.71,"sector":"Consumer Defensive"},"dividends":[{"exDate":"2017-08-30","amount":0.54},{"exDate":"2017-05-30","amount":0.52},{"exDate":"2017-02-27","amount":0.52},{"exDate":"2016-11-29","amount":0.52}]}}';
    $infojson = (decode_json $stockdata)[0];
    # Jedes Symbol untersuchen
    while (my ($symbol, $areas) = each %{$infojson}) {
      my $kursptr = $self->{Kurs}->{$symbol};
      next if !$kursptr;                           # keins von unseren Symbolen; sollte nicht passieren
      Trace->Trc('S', 1, 0x00001, "Untersuche Symbol: $symbol");

      # Alle neuen infos auswerten und zwischensteichern
      my $info;
      while (my ($area, $myFlags) = each %{$areas}) {
        if (ref($myFlags) eq 'ARRAY') {
          Trace->Trc('S', 1, 0x00001, "Untersuche Flag: dividend");
          foreach my $flag (@{$myFlags}) {
            $info->{$nameMap{amount}} += $flag->{amount} if $flag->{amount};
            if (defined ($flag->{exDate})) {
              my ($y, $m, $d) = split(/-/, $flag->{exDate});
              $info->{$nameMap{exDate}} = ++$y ."-$m-$d";
            }
          }
        } else {
          while (my ($flag, $value) = each %{$myFlags}) {
            Trace->Trc('S', 1, 0x00001, "Untersuche Flag: $flag");
            $info->{$nameMap{$flag}} = $value;
            if (my $faktor = $self->{Flags}->{$nameMap{$flag}}->{iExtrading}->{Faktor}) {
              $info->{$nameMap{$flag}} *= $faktor;
            }
          }
        }
      }

      # Weiter, falls wir bereits neuere Infos haben
      next if defined($kursptr->{Last_Trade_TS}) && defined($info->{Last_Trade_TS}) && ($info->{Last_Trade_TS} < $kursptr->{Last_Trade_TS});

      # Infos uebernehen
      $kursptr->{aktuell} = 1;
      $kursptr->{letzter_Abruf} = str2time(gmtime());
      $kursptr->{Currency} = $kursptr->{Dividend_Currency} = 'USD';    # dies Quelle liefert nur USD Infos
      while (my ($flag, $value) = each(%{$info})) {
        Trace->Trc('I', 5, 0x02102, $symbol, $flag, $value);
        $kursptr->{$flag} = $value;
      }
      if ($kursptr->{Last_Trade_TS}) {
        $kursptr->{Last_Trade_TS} = int($kursptr->{Last_Trade_TS});
        $kursptr->{Last_Trade_Time} = time2str('%R', $kursptr->{Last_Trade_TS} + $self->{UTCDelta});
        $kursptr->{Last_Trade} = time2str('%d.%m.%y %R', $kursptr->{Last_Trade_TS} + $self->{UTCDelta});
        $kursptr->{Last_Trade_Days_ago} = abs((str2time(time2str('%x', time)) - str2time(time2str('%x', $info->{Last_Trade_TS}))) / 86400);
      }

      if ($kursptr->{Name}) {$kursptr->{Name} =~ s/\s+N$//}
      if (defined($kursptr->{Dividend})) {$kursptr->{Dividend} ||= 0}
      if (!defined($kursptr->{Symbol}) || ($kursptr->{Symbol} eq '')) {$kursptr->{Symbol} = $symbol}
    }
    # Kursinfos speichern
    if (exists($self->{VarFile})) {
      store $self->{Kurs}, $self->{VarFile}
    }
  }

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return 0;
} ## end sub _webabruf_iExtrading


sub _webabruf_Stooq {
  #################################################################
  # Durchfuehren eines Webabrufs
  my $self = shift;

  my $symbols = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 3, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;

  my %month = ('sty' => 'Jan', 'lut' => 'Jan', 'mar' => 'Mar', 'kwi' => 'Apr',
               'maj' => 'Mai', 'cze' => 'Jun', 'lip' => 'Jul', 'aug' => 'Aug',
               'wrz' => 'Sep', 'paz' => 'Oct', 'lis' => 'Nov', 'gru' => 'Dec',);

  STOCK: foreach my $position (sort @{$symbols}) {
    my $kursptr = $self->{Kurs}{$position};
    my @posSymbols = split('\|', $position);
    SYMBOL: while ((my $symbol = shift(@posSymbols)) && !($kursptr->{last})) {
      # Fuer jedes Symbol den Abruf machen
      Trace->Trc('I', 2, 0x02104, $symbol);
      my $webAbruf = Utils::extendString(Configuration->config('Stockservice Stooq', 'URL'), 'SYMBOL|' . $symbol);
      Trace->Trc('I', 2, 0x02106, $webAbruf);
      my $stockdata = get($webAbruf);
      my $tree = HTML::TreeBuilder->new();
      $tree->parse($stockdata);
      $tree->eof();

      my $info;
      my $title = $tree->look_down('_tag' => 'title');
      my $name = $title->as_text;
      next SYMBOL if $name !~ /^[^\-]*?\-\s(.*)\s\-[^\-]*?$/;
      $info->{Name} = $1;

      FLAGS: while (my ($flag, $value) = each(%{$self->{Flags}})) {
        next FLAGS if !exists($value->{Stooq});
        my ($tag, $id) = split(/_/, $value->{Stooq}->{Name});
        $id = 'aq_' . lc($position) . "_$id" if $tag eq 'span';
        next FLAGS if !defined($id);
        my $faktor = $value->{Stooq}->{Faktor};
        my @nodes = $tree->look_down('_tag' => $tag);
        my $parameter;
        NODE: foreach my $node (@nodes) {
          my $nodeid = $node->attr('id');
          next NODE if !$nodeid || $nodeid !~ /$id/;
          if ($flag eq 'Dividend') {
            $parameter = $node->as_text;
          } else {
            $parameter = $node->as_text;
          }
        }
        next FLAGS if !defined($parameter);
        if ($parameter =~ /^\((.*)\)$/) {
          $parameter = $1;
        }
        $info->{$flag} = $parameter;
      }

      # Weiter, falls wir bereits neuere Infos haben
      next SYMBOL if defined($kursptr->{Last_Trade_TS}) && defined($info->{Last_Trade_TS}) && ($info->{Last_Trade_TS} < $kursptr->{Last_Trade_TS});

      # Infos uebernehen
      $kursptr->{aktuell} = 1;
      $kursptr->{letzter_Abruf} = str2time(gmtime());
      while (my ($flag, $value) = each(%{$info})) {
        Trace->Trc('I', 5, 0x02102, $symbol, $flag, $value);
        $kursptr->{$flag} = $value;
      }

      foreach (keys(%month)) {
        $kursptr->{Last_Trade_Date} =~ s/$_/$month{$_}/;
      }
      my ($sec,$min,$hour,$mday,$mon,$year,$wday,$yday,$isdst) = localtime();
      $year = $year+1900;
      $kursptr->{Last_Trade_TS} = str2time("$kursptr->{Last_Trade_Date} $year $kursptr->{Last_Trade_Time}", $self->{TZ}) || 0;

      if ($kursptr->{Last_Trade_TS}) {
        $kursptr->{Last_Trade_TS} = int($kursptr->{Last_Trade_TS});
        $kursptr->{Last_Trade_Time} = time2str('%R', $kursptr->{Last_Trade_TS} + $self->{UTCDelta});
        $kursptr->{Last_Trade} = time2str('%d.%m.%y %R', $kursptr->{Last_Trade_TS} + $self->{UTCDelta});
        $kursptr->{Last_Trade_Days_ago} = abs((str2time(time2str('%x', time)) - str2time(time2str('%x', $kursptr->{Last_Trade_TS}))) / 86400);
      }

      if ($kursptr->{Name}) {$kursptr->{Name} =~ s/\s+N$//}
      if (defined($kursptr->{Dividend})) {$kursptr->{Dividend} ||= 0}
      if (!defined($kursptr->{Symbol}) || ($kursptr->{Symbol} eq '')) {$kursptr->{Symbol} = $symbol}
    }
  }
  # Kursinfos speichern
  if (exists($self->{VarFile})) {
    store $self->{Kurs}, $self->{VarFile}
  }

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return 0;
} ## end sub _webabruf_Stooq


sub _webabruf_YAHOO {
  #################################################################
  # Durchfuehren eines Webabrufs
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 3, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;

  my $rc = 0;

  my %hashTemp = map {$_ => 1} (keys %{$self->{Flags}}, ('last', 'date', 'time'));
  my @flags = sort(keys %hashTemp);
  Trace->Trc('I', 2, 0x02101, join(' ', @flags));

  foreach (sort keys %{$self->{Kurs}}) {
    my @symbols = split('\|', $_);
    my $kursptr = $self->{Kurs}{$_};
    while ((my $symbol = shift(@symbols)) && !($kursptr->{last})) {
      # Fuer jedes Symbol den Abruf machen sofern noch kein Kurs feststeht
      Trace->Trc('I', 2, 0x02104, $symbol);
      if (!$kursptr->{_aktuell} && (!$kursptr->{_letzter_Abruf} || ($kursptr->{_letzter_Abruf} < time - $self->{MaxDelay}))) {
        my $webAbruf = Utils::extendString(Configuration->config('Stockservice Yahoo', 'URL'), 'STOCK|' . $symbol . '|DATA|' . join('', @flags));
        Trace->Trc('I', 2, 0x02106, $webAbruf);
        my $maxtry = Configuration->config('Stockservice Yahoo', 'Anzahl_Versuche') || 10;

        my $crlfmerker = $/;
        $/ = '%';

        $kursptr->{_aktuell} = 0;
        do {
          my $stockdata = get($webAbruf);
          if (defined($stockdata) && $stockdata !~ 'Missing') {
            # die "Failed to get Stock Data" unless defined $stockdata;
            my @myFlags   = @flags;
            while ($stockdata =~ /\"[\s0]*([^\"]+?)[\s]*\"|[\s0]*([^,\s]+)/g) {
              chomp(my $wert = $1 ? $1 : $2);
              $wert        = 0 if ($wert eq 'N/A');
              my $flagname = shift(@myFlags);
              my $flag     = $self->{Flags}{$flagname}{Abbr};
              if (defined($flag) && $flag ne '-') {
                # Sichern des Originalwertes
                if ($self->{Flags}{$flagname}{Faktor}) {
                  $kursptr->{$flagname . '_RAW'} = $wert
                }
                if ($self->{Flags}{$flagname}{Faktor} && $kursptr->{_Waehrung}) {
                  if ($self->{Flags}{$flagname}{Faktor} == 1) {
                    my $curxchg = $self->{BasisCur} . $kursptr->{_Waehrung} . '=X';
                    # Falls der Wechselkurs fuer die Waehrung noch nicht ermittelt ist,
                    # versuchen wir das
                    if (!$self->{Kurs}{$curxchg}{last}) {
                      # Default -> Wechselkurs 1
                      $self->{Kurs}{$curxchg}{last} = 1;
                      if ($kursptr->{_Waehrung} ne $self->{BasisCur}) {
                        if ($kursptr->{_Waehrung} =~ /^[0-9.,]+/) {
                          # Keine Standardwaehrung -> Wechselkurs wird direkt angegeben
                          $self->{Kurs}{$curxchg}{last} = $kursptr->{_Waehrung};
                        } elsif ($kursptr->{_Waehrung} =~ /^[A-Za-z][A-Za-z0-9]{2}/) {
                          # Standardwaehrung -> Wechselkurs wird ermittelt
                          $self->_webabruf_YAHOO($curxchg, $self->{Kurs}{$curxchg});
                          # Normalisieren des Wertes
                          $self->{Kurs}{$curxchg}{last} *= 1;
                          # Fuer GBP ist ein weiterer Faktor 100 noetig
                          $self->{Kurs}{$curxchg}{last} *= 100 if ($kursptr->{_Waehrung} eq 'GBP');
                        }
                      } ## end if ($kursptr->{_Waehrung...})
                    } ## end if (!$self->{Kurs}{$curxchg...})
                    $self->{Exchange}{$kursptr->{_Waehrung}} = $self->{Kurs}{$curxchg}{last};

                    # Der Wert in Basiswaehrung wird nur ermittelt, falls ein Wechselkurs existiert
                    $wert = $wert / $self->{Kurs}{$curxchg}{last} if ($self->{Kurs}{$curxchg}{last});
                  } else {
                    $wert = $wert * $self->{Flags}{$flag}{Faktor};
                  }
                } ## end if ($self->{Flags}{$flag...})
                # Sichern des faktorisierten Wertes
                $kursptr->{$flagname} = ($flagname ne 'symbol') ? $wert : (split(/\./, $wert))[0];
              } ## end if (defined($flag))
            } ## end while ($stockdata =~ /\"[\s0]*([^\"]+?)[\s]*\"|[\s0]*([^,\s]+)/g)
          } ## end if (defined($stockdata...))
          $maxtry--;
        }
        until ($maxtry <= 0 || ($kursptr->{last} &&
                                $kursptr->{last} ne 'N/A' &&
                                $kursptr->{last} > 0 &&
                                $kursptr->{last} <= 1000000 &&
                                $kursptr->{last} != 1));

        if ($kursptr->{date} && $kursptr->{'time'}) {
          $kursptr->{Last_Trade_TS} = str2time("$kursptr->{date} $kursptr->{'time'}", $self->{TZ}) || 0;
          $kursptr->{Last_Trade}    = time2str('%d.%m.%y %R', $kursptr->{Last_Trade_TS});
          $kursptr->{_lastTrade}    = abs((str2time(time2str('%x', time)) - str2time(time2str('%x', $kursptr->{Last_Trade_TS}))) / 86400);
          if ($kursptr->{Last_Trade_TS} < str2time(time2str('%x', time))) {
            # $kursptr->{Last_Trade} .= '<';
            $kursptr->{_aktuell} = ($kursptr->{_letzter_Abruf} && $kursptr->{_letzter_Abruf} < time - $self->{MaxDelay}) ? 0 : 1;
          } else {
            $kursptr->{_aktuell} = 1
          }
          $kursptr->{_letzter_Abruf} = time;
        } else {
          $kursptr->{Last_Trade} = 0
        }

        if ($kursptr->{name}) {
          $kursptr->{name} =~ s/\s+N$//
        }

        Trace->Trc('I', 2, 0x02102, $symbol, $kursptr->{name}, $kursptr->{last});

        $/ = $crlfmerker;
      } ## end if (!$kursptr->{_aktuell...})

      if ($kursptr->{_last}) {
        $kursptr->{last} = $kursptr->{_last}
      }

      # Kursinfos speichern
      if (exists($self->{VarFile})) {
        store $self->{Kurs}, $self->{VarFile}
      }
    }
  }

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub _webabruf_YAHOO


sub _webabruf_QUOTE {
  #################################################################
  # Durchfuehren eines Webabrufs
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 3, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;

  my $quoter = Finance::Quote->new();
  $quoter->set_currency($self->{BasisCur});  # Set default currency.
  # Wechselkurse ermitteln
  foreach my $symbol (keys(%{$self->{Kurs}})) {
    next if !$self->{Kurs}->{$symbol}->{exchange};
    $self->{Kurs}->{$symbol}->{price} = $quoter->currency($symbol);
  }

  # Array ueber alle Symbole erstellen
  my @markets = split(' ', Configuration->config('Stockservice QUOTE', 'Markets'));
  if (!@markets) {@markets = (qw/vwd europe/)};

  foreach my $quelle (@markets) {
    Trace->Trc('I', 2, 0x02105, $quelle);
    my @symbols;
    foreach my $symbol (sort keys %{$self->{Kurs}}) {
      if (!$self->{Kurs}{$symbol}{aktuell} &&
          (!$self->{Kurs}{$symbol}{letzter_Abruf} ||
           ($self->{Kurs}{$symbol}{letzter_Abruf} < time - $self->{MaxDelay}))) {
        push(@symbols, $symbol)
      }
    }
    if (scalar(@symbols)) {
      Trace->Trc('I', 2, 0x02104, join(' ', @symbols));
      my %info = $quoter->fetch($quelle, @symbols);

      foreach my $symbol (@symbols) {
        next if !$info{$symbol,"success"};       # Skip failures.
        next if $info{$symbol,"last"} eq '0,00'; # Skip failures.
        my $kursptr = $self->{Kurs}{$symbol};
        my %posval;
        foreach (keys %{$self->{Flags}}) {
          # Ergebnisse auswerten
          my $infoval = $self->{Flags}{$_}{Abbr} ? $info{$symbol, $self->{Flags}{$_}{Abbr}} : undef;
          if (!defined($infoval)) {
            $infoval = $self->{Flags}{$_}{Default};
          }
          if (defined($infoval) && $self->{Flags}{$_}{Laenge}) {
            $infoval = substr($infoval, 0, $self->{Flags}{$_}{Laenge})
          }
          $posval{$_} = $infoval;
          Trace->Trc('I', 5, 0x02102, $symbol, $_, $posval{$_});
        }

        # $self->{Kurs} aktualisieren, falls das Ergebnis neuer ist
        $posval{Last_Trade_TS} = 0;
        if ($posval{Last_Trade_Date} && $posval{Last_Trade_Time}) {
          $posval{Last_Trade_TS} = str2time("$posval{Last_Trade_Date} $posval{Last_Trade_Time}") || 0
        }

        if (!defined($kursptr->{Last_Trade_TS}) || ($posval{Last_Trade_TS} > $kursptr->{Last_Trade_TS})) {
          $kursptr->{aktuell} = 0;

          foreach my $flagname (keys(%posval)) {
            if ($flagname eq 'Symbol') {
              $kursptr->{$flagname} = (split(/\./, $posval{$flagname}))[0];
            } else {
              $kursptr->{$flagname} = $posval{$flagname};
            }
          }

          if ($posval{Last_Trade_TS}) {
            $kursptr->{Last_Trade} = time2str('%d.%m.%y %R', $kursptr->{Last_Trade_TS} + $self->{UTCDelta});
            $kursptr->{Last_Trade_Days_ago} = abs((str2time(time2str('%x', time)) - str2time(time2str('%x', $posval{Last_Trade_TS}))) / 86400);
            $kursptr->{aktuell} = 1;
            $kursptr->{letzter_Abruf} = str2time(gmtime());
          } else {
            $kursptr->{Last_Trade} = 0
          }

          if ($kursptr->{Name}) {$kursptr->{Name} =~ s/\s+N$//}
          if (defined($kursptr->{Dividend})) {$kursptr->{Dividend} ||= 0}
          if (!defined($kursptr->{Symbol}) || ($kursptr->{Symbol} eq '')) {$kursptr->{Symbol} = $symbol}
          if (!defined($kursptr->{Stock_Exchange}) || ($kursptr->{Stock_Exchange} eq '')) {$kursptr->{Stock_Exchange} = $quelle}
        }
        Trace->Trc('I', 5, 0x02102, $symbol, $kursptr->{Name}, $kursptr->{Last_Trade_Days_ago});
      }
    }
  }

  # Kursinfos speichern
  if (exists($self->{VarFile})) {
    store $self->{Kurs}, $self->{VarFile}
  }

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return 0;
} ## end sub _webabruf_QUOTE


sub Flags_laden {
  #####################################################################
  # Laden der Flags aus der INI-Datei
  # Anlegen des DEFAULT Kusreintrags
  #
  # Ergebnis: Datenstruktur $self->{Flags} mit folgendem Aufbau
  # $self->{Flags}->{<Flagname>}
  #     Faktor  : Anpassungsfaktor
  #     Default : Defaultwert
  #     Format  : printf Formatstring

  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  # Anlegen der mandatorischen Kursfelder

  # Aufbau der Info Flags
  my %flagInfo = Configuration->config('Flags');
  foreach my $flagName (keys %flagInfo) {
    my ($faktor, $format, $default) = split(' ', $flagInfo{$flagName});
    $faktor = 0      if (!defined($faktor)  || $faktor eq '-');
    $format = undef  if (!defined($format)  || $format eq '-');
    $default = undef if (!defined($default) || $default eq '-');
    $self->{Flags}->{$flagName} = {Faktor  => $faktor,
                                   Default => $default,
                                   Format  => $format};
  }

  foreach my $service (@{$self->{Services}}) {
    my %namemapping = Configuration->config('Stockservice '.$service);
    foreach my $key (keys %namemapping) {
      next if !$namemapping{$key};
      next if $key !~ /^map (.*)$/;
      my $flagName = $1;
      next if !exists($self->{Flags}->{$flagName});
      my ($serviceFlagName, $factor) = split (/ /, $namemapping{$key});
      next if !exists($self->{Flags}->{$flagName});
      $self->{Flags}->{$flagName}->{$service}->{Name} = $serviceFlagName;
      $self->{Flags}->{$flagName}->{$service}->{Faktor} = $factor if defined($factor);
    }
  }

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return 0;
} ## end sub Flags_laden


sub Portofolios_lesen {
  #####################################################################
  # Einlesen der Portofolios mit Inhalten
  # Ergebnis: Datenstruktur $self->{Portofolios} mit folgendem Aufbau
  # $self->{Portofolios}->{<Portotfolioname>}->{<Positionsname>}
  #     Branche       : Branche
  #     Quantity      : Amount of Shares
  #     Dividend      : Dividende/Share
  #     Price_Buy_Pos : Buying Price of whole Position
  #     Price_Buy     : Buying Price/Share
  # und
  # $self->{Portofolios}->{<Portotfolioname>}->{<Positionsname>}
  #     Branche       : Branche
  #     Quantity      : Amount of Shares
  #     Dividend      : Dividende/Share
  #     Price_Buy_Pos : Buying Price of whole Position
  #     Price_Buy     : Buying Price/Share
  # und
  # $self->{Portofolios}->{<Portotfolioname>}->{CASH}
  #    Cashbestand des Portofolios

  #
  # Prozedurnummer 3
  #
  my $self = shift;
  my $singlePF = shift || '';

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  # Sammel alle Konfigdateien ein
  my %PFHash;   # Hash mit allen Portofolios und Watchlists (komplett)

  if (Configuration->config('Eingabedatei')) {
    my %PFFiles = Configuration->config('Eingabedatei');
    foreach (keys(%PFFiles)) {
      Trace->Trc('I', 2, 0x02201, $PFFiles{$_});
      my %PFList;
      if (-r $PFFiles{$_} && tie(%PFList, 'Config::IniFiles', (-file => $PFFiles{$_}))) {
        foreach my $current (keys %PFList) {
          # Sammle alle Portofolios oder Watchlists inklusive Namen aus den Konfigurationsfiles
#          if ($current =~ /^(Portofolio|Watchlist) .*$/) {
            Trace->Trc('I', 2, 0x02202, $PFFiles{$_}, $current);
            if (defined($PFHash{$current})) {
              # Portofolio oder Teile davon wurden schon mal vorher definiert
              foreach my $pos (keys %{$PFList{$current}}) {
                Trace->Trc('I', 2, 0x02203, $PFFiles{$_}, $current, $pos);
                if (defined($PFHash{$current}{$pos})) {
                  # Die spezielle Position ist schon definiert -> anhaengen -> POSITION WIRD ZU EINEM ARRAY, FALLS SIE NOCH KEINER IST
                  my @dummy;
                  if (ref($PFHash{$current}{$pos})) {
                    @dummy = @{$PFHash{$current}{$pos}};
                  } else {
                    push(@dummy, $PFHash{$current}{$pos});
                  }
                  if (ref($PFList{$current}{$pos})) {
                    push(@dummy, @{$PFList{$current}{$pos}});
                  } else {
                    # Falls die Position nur einmal vorkommt schieb das Element in den Array
                    push(@dummy, $PFList{$current}{$pos});
                  }
                  $PFHash{$current}{$pos} = \@dummy;
                } else {
                  # Die spezielle Position ist nocht nicht definiert -> einfach uebernehmen
                  $PFHash{$current}{$pos} = $PFList{$current}{$pos};
                }
              } ## end foreach my $pos (keys %{$PFList...})
            } else {
              $PFHash{$current} = $PFList{$current};
            }
#          } ## end if ($current...)
        } ## end foreach my $current...
      } ## end if (-r $PFFiles...)
    } ## end foreach (keys(%PFFiles...))
  } ## end if (Configuration->conf...

  # Falls singlePF angegeben ist, wird nur dieses bahandelt
  if ($singlePF) {
    my $dummy = $PFHash{$singlePF};
    undef(%PFHash);
    $PFHash{$singlePF} = $dummy;
    $self->{Gesamtliste} = undef;
  }
  $self->{PFHash} = \%PFHash;

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Portofolios_lesen


sub Cash_extrahieren {
  #####################################################################
  # Extrahiert die CASH Eintraege

  #
  # Prozedurnummer 3
  #
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  my %tmpHash = %{$self->{PFHash}};
  # Extraktion der CASH Eintraege in eine eigene Liste
  my %cash;
  my %PFHash;
  foreach my $PFName (keys(%tmpHash)) {
    foreach my $pos (keys %{$tmpHash{$PFName}}) {
      if (($PFName !~ /^Cash/) && ($pos !~ /^CASH/)) {
        Trace->Trc('I', 5, "Bearbeite aus Portofolio <%s> Share-Position <%s>.", $PFName, $pos);
        $PFHash{$PFName}{$pos} = $tmpHash{$PFName}{$pos};
      } else {
        Trace->Trc('I', 5, "Bearbeite aus Portofolio <%s> Cash-Position <%s>.", $PFName, $pos);
        if ($pos =~ /^(CASH\.)?(.*)\.(.*)$/) {
          my ($owner, $bank) = ($2, $3);
          if (defined($owner) && defined($bank)) {
            Trace->Trc('I', 2, 0x02212, $PFName, $pos, $owner, $bank);
            my @valuearr = ref($tmpHash{$PFName}{$pos}) ? @{$tmpHash{$PFName}{$pos}} : ($tmpHash{$PFName}{$pos});
            foreach (@valuearr) {
              Trace->Trc('I', 5, "Parse line <$_>");
              my ($cur, $value);
              if ($_ =~ /^\{.*\}$/) {
                # JSON Syntax
                my $attrHash = decode_json $_;
                $cur   = $attrHash->{Currency} || $self->{BasisCur};
                $value = $attrHash->{Cash}     || 0;
              } else {
                # Old Syntax EUR|12345.00 alternativ nur 12345.00
                ($cur, $value) = split(/\|/, $_);
                if (!defined($value)) {
                  $value = $cur;
                  $cur   = $self->{BasisCur};
                }
              }
              if (defined(code2currency($cur))) {
                $cash{$owner}{$bank}{$cur} += $value;
                $cash{$owner}{Summe}{$cur} += $value;
                $cash{Bank}{$bank}{$cur}   += $value;
                $cash{Bank}{Summe}{$cur}   += $value;
              } else {
                Trace->Trc('I', 2, 0x02213, $cur);
              }
            }
          }
        }
      }
    }
  }
  $self->{Cash}   = \%cash;
  $self->{PFHash} = \%PFHash;

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Cash_extrahieren


sub Gesamtliste_erzeugen {
  #####################################################################
  # Erzeugt die Gesamtliste

  #
  # Prozedurnummer 3
  #
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  my %PFHash = %{$self->{PFHash}};

  # Ggf. Gesamtliste als Watchlist anlegen
  my %gesamtliste;
  foreach my $PFName (keys %PFHash) {
    if ($PFName !~ /^Watchlist .*$/) {
      foreach my $pos (keys %{$PFHash{$PFName}}) {
        Trace->Trc('I', 5, 0x02203, $PFName, $pos);
        if (defined($gesamtliste{$pos})) {
          # Die spezielle Position ist schon definiert -> anhaengen -> POSITION WIRD ZU EINEM ARRAY, FALLS SIE NOCH KEINER IST
          my @dummy;
          if (ref($gesamtliste{$pos})) {
            @dummy = @{$gesamtliste{$pos}};
          } else {
            push(@dummy, $gesamtliste{$pos});
          }
          if (ref($PFHash{$PFName}{$pos})) {
            push(@dummy, @{$PFHash{$PFName}{$pos}});
          } else {
            # Falls die Position nur einmal vorkommt schieb das Element in den Array
            push(@dummy, $PFHash{$PFName}{$pos});
          }
          $gesamtliste{$pos} = \@dummy;
        } else {
          # Die spezielle Position ist nocht nicht definiert -> einfach uebernehmen
          $gesamtliste{$pos} = $PFHash{$PFName}{$pos};
        }
      }
    }
  }
  $PFHash{"Watchlist Summe"} = \%gesamtliste;

  $self->{PFHash} = \%PFHash;

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Gesamtliste_erzeugen


sub Positionen_parsen {
  #####################################################################
  # Parse die Eintraege fuer alle Positionen

  #
  # Prozedurnummer 3
  #
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  my %PFHash = %{$self->{PFHash}};
  delete($self->{PFHash});

  # Datenstruktur zum Sammeln der Kursinformatinen zu allen Positionen anlegen
  # und mit Werten aus der INI fuellen
  foreach my $PFName (keys(%PFHash)) {
    # Fuer jedes gefundene Protofolio/jede Watchlist, der/die Elemente (Aktienpositionen) enthaelt
    if (scalar keys %{$PFHash{$PFName}}) {
      foreach my $pos_alternativs (sort keys %{$PFHash{$PFName}}) {
        my $pos_alternativs_sort = join("|", sort(split(/\|/, $pos_alternativs)));
        my $pos = (split(/\./, $pos_alternativs_sort))[0];
        # Fuer jede Aktienposition
        # Falls die Position mehrfach vorkommt sammle alle Vorkommen in einem Array
        # Falls die Position nur einmal vorkommt schieb das Element in den Array
        my @stockattributearray = ref($PFHash{$PFName}{$pos_alternativs}) ? @{$PFHash{$PFName}{$pos_alternativs}} : $PFHash{$PFName}{$pos_alternativs};
        # Im Array @stockattributearray liegen ein oder mehrere Elemente
        # Uebernahme der Elemente in das Portofolio und Setzen der Defaults
        my $poscount = 1;
        foreach my $exg_anz_prz (@stockattributearray) {
          # Ermitteln den eindeutigen Index
          Trace->Trc('I', 5, "Parse line <$exg_anz_prz>");
          my $pos_count = "$pos $poscount";
          while (defined($self->{Portofolios}{$PFName}{$pos_count})) {
            $poscount++;
            $pos_count = "$pos $poscount";
          }
          # Belegen der Default Werte
          my $attrHash;
          foreach (keys(%{$self->{Flags}})) {
            $attrHash->{$_} = $self->{Flags}{$_}{Default};
          }
          $attrHash->{Symbol}              = $pos;
          $attrHash->{Symbol_Alternatives} = $pos_alternativs_sort;

          my @attrArr = split(/\|/, $exg_anz_prz);
          if (scalar @attrArr > 1) {
            # Alte Notation: CAD|740|10433.2|0.147|Rohstoff||
            $attrHash->{Currency}          = $attrArr[0] || $self->{BasisCur};
            if (!defined(code2currency($attrHash->{Currency}))) {
              Trace->Trc('I', 2, 0x02213, $attrHash->{Currency});
              next;
            }
            $attrHash->{Dividend_Currency} = $attrHash->{Currency};
            $attrHash->{Quantity}          = $attrArr[1] || 0;
            $attrHash->{Price_Buy_Pos}     = $attrArr[2];
            $attrHash->{Dividend}          = $attrArr[3] || '0';
            $attrHash->{Branche}           = $attrArr[4] || '';
            $attrHash->{Price}             = (defined($attrArr[5]) && $attrArr[5] =~ m/^([-+]?)([0-9]*).*$/) ? $attrArr[5] : 0;
          } else {
            # Neue JSON Notation: {Currency:"CAD", Quantity:740, Price_Buy_Pos:10433.2, Dividend:0.147, Branche:"Rohstoff", WKN:1234567,  Name:"Willi was here"}
            my $json = decode_json $exg_anz_prz;
            foreach (keys(%{$json})) {
              $attrHash->{$_} = $json->{$_};
            }
            if (!defined($attrHash->{Price_Buy_Pos}) && defined($attrHash->{Price_Buy}) && defined($attrHash->{Quantity}) && looks_like_number($attrHash->{Quantity})) {
              $attrHash->{Price_Buy_Pos} = $attrHash->{Quantity} * $attrHash->{Price_Buy};
            }
            $attrHash->{Currency}          ||= $self->{BasisCur};
            if (!defined(code2currency($attrHash->{Currency}))) {
              Trace->Trc('I', 2, 0x02213, $attrHash->{Currency});
              next;
            }
            $attrHash->{Dividend_Currency} ||= $attrHash->{Currency};
            if (!defined(code2currency($attrHash->{Dividend_Currency}))) {
              Trace->Trc('I', 2, 0x02213, $attrHash->{Dividend_Currency});
              next;
            }
          }

          # Belegen der Kurswerte
          foreach my $alternative (split(/\|/, $pos_alternativs)) {
            if (!defined($self->{Kurs}{$alternative})) {
              foreach my $attribute (keys(%{$attrHash})) {
                $self->{Kurs}{$alternative}{$attribute} = $attrHash->{$attribute};
              }
            }
          }

          # Uebernehmen der ermittelten Werte
          foreach (keys(%{$attrHash})) {
            $self->{Portofolios}{$PFName}{$pos_count}{$_} = defined($attrHash->{$_}) ? $attrHash->{$_} : '';
          }
        } ## end foreach my $exg_anz_prz (@stockattributearray)
      } ## end foreach my $pos (keys %{$PFHash...})
    } ## end if (scalar keys %{$PFHash...})
  } ## end foreach my $PFName ...

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Positionen_parsen


sub Wechselkurse_lesen {
  #################################################################
  # Ergaenzt den Kurshash um die Wechselkurseintraege
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  my $basis = $self->{BasisCur};

  my %newCStocks;
  foreach my $cur (keys %{$self->{Cash}->{Bank}->{Summe}}) {
    next if ($cur eq $basis);
    foreach my $exchange ($basis . $cur, $cur . $basis) {
      $newCStocks{$exchange}->{Symbol}   = $exchange;
      $newCStocks{$exchange}->{aktuell}  = 0;
      $newCStocks{$exchange}->{exchange} = 1;
    }
  }
  foreach my $symbol (keys %{$self->{Kurs}}) {
    foreach my $attribute ('Currency', 'Dividend_Currency', 'Währung') {
      my $cur = $self->{Kurs}{$symbol}{$attribute} || $basis;
      next if ($cur eq $basis);
      next if $cur !~ /^[0-0A-Z]{3}$/;
      foreach my $exchange ($basis . $cur, $cur . $basis) {
        $newCStocks{$exchange}->{Symbol} = $exchange;
        $newCStocks{$exchange}->{aktuell} = 0;
        $newCStocks{$exchange}->{exchange} = 1;
      }
    }
  }
  foreach my $symbol (keys %newCStocks) {
    $self->{Kurs}->{$symbol} = $newCStocks{$symbol};
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Wechselkurse_lesen


sub Kurse_ermitteln {
  #################################################################
  # Ermittelt Kurse zu allen Papieren, die in den Depots gefunden
  # wurden
  # Ergebnis: In der Datenstruktur $self->{Kurse} liegt
  # ein Hash (Portofolios) von Hashes (Position) mit:
  #
  # Quantity          : Amount of Shares in Position

  # Price_Buy_Pos     : Buying Price of whole Position
  # Price_Buy         : Buying Price for one Share

  #
  # Prozedurnummer 2
  #
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  # Kurs ermitteln
  # Ergebnis ist in $self->{Kurs} abgelegt

  my $retry = $self->{Retry};
  my (@symbols, @oldnoexsymbols, @noexsymbols);
  RETRY: while ($retry) {
    SERVICE: foreach my $service (@{$self->{Services}}) {
      my (%symbolhash, %noexsymbolhash);
      SHARE: while (my ($symbol, $kurs) = each %{$self->{Kurs}}) {
        next SHARE if $kurs->{aktuell};
        next SHARE if $kurs->{letzter_Abruf} && ($kurs->{letzter_Abruf} > time-$self->{MaxDelay});
        $symbolhash{$symbol} = 1;
        next SHARE if $kurs->{exchange};
        $noexsymbolhash{$symbol} = 1;
      }
      @oldnoexsymbols = @noexsymbols;
      @noexsymbols = sort(keys(%noexsymbolhash));
      @symbols = sort(keys(%symbolhash));
      last RETRY if !scalar(@symbols);
      eval "\$self->_webabruf_$service(\\\@symbols, $retry)";
    }
    if (scalar(@noexsymbols) eq scalar(@oldnoexsymbols)) {
      $retry--;
    } else {
      $retry = $self->{Retry};
    }
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Kurse_ermitteln


sub Reverse_Kurs {
  #################################################################
  # Ermittelt reverse Kurse den exchange Rates
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  EXCHANGERATE: foreach my $symbol (keys(%{$self->{Kurs}})) {
    my $cur = $self->{Kurs}->{$symbol};
    next EXCHANGERATE if !$cur->{exchange};
    next EXCHANGERATE if !$cur->{aktuell};
    my $rev = $self->{Kurs}->{substr($symbol, 3) . substr($symbol, 0, 3)};
    next EXCHANGERATE if $rev->{aktuell};
#    $rev = clone($cur);
    foreach my $key (keys %{$cur}) {
      $rev->{$key} = $cur->{$key} if !defined($rev->{$key});
    }
    $rev->{Price} = $cur->{Price} ? 1/$cur->{Price} : 0;
    $rev->{Change_Day} = $cur->{Price} && ($cur->{Price} != $cur->{Change_Day}) ? 1/$cur->{Price} - 1/($cur->{Price}-$cur->{Change_Day}) : 0;
    $rev->{Change_Day_Percent} = $cur->{Price} ? -100 * $cur->{Change_Day}/$cur->{Price} : 0;
    $rev->{aktuell}        = 1;
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Reverse_Kurs


sub Kurse_umrechnen {
  #################################################################
  # Ergaenze Kursinfo durch Bereinigungen und Wechselkursumrechnung
  #
  # Kurse sollten abgeholt und in $self->{Kurs} gespeichert sein
  # jetzt bereinigen wir die geladenen Werte und setzen ggf. Defaults
  # Ausserdem werden Waehrungsfelder ggf. umgerechnet

  my $self = shift;

  sub max ($$) { $_[$_[0] < $_[1]] }
  sub min ($$) { $_[$_[0] > $_[1]] }

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  # Umrechnen aller Portofolio Positionen
  foreach my $PFName (keys(%{$self->{Portofolios}})) {
    foreach my $pos (keys %{$self->{Portofolios}{$PFName}}) {
      my $position = $self->{Portofolios}{$PFName}{$pos};

      if ($position->{Currency} ne $self->{BasisCur}) {
        # Gekauft in einer anderen Waehrung als jetzt dargestellt
        foreach my $attribute ('Price_Buy_Pos') {
          if ($position->{$attribute}) {
            if (my $rate = $self->{Kurs}->{$position->{Currency} . $self->{BasisCur}}->{Price}) {
              $position->{$attribute} *= $rate;
            }
          }
        }
        $position->{Currency} = $self->{BasisCur};
      }
      if ($position->{Dividend_Currency} ne $self->{BasisCur}) {
        if (my $rate = $self->{Kurs}->{$position->{Dividend_Currency} . $self->{BasisCur}}->{Price}) {
          foreach my $attribute ('Dividend') {
            if ($position->{$attribute}) {
              $position->{$attribute} *= $rate;
            }
          }
          $position->{Dividend_Currency} = $self->{BasisCur};
        }
      }

      # Faktorisieren der Felder falls konfiguriert
      foreach my $attribute (keys(%{$position})) {
        if ($self->{Flags}{$attribute}{Faktor}) {
          $position->{$attribute} *= $self->{Flags}{$attribute}{Faktor};
        }
      }
    }
  }

  # Umrechnen aller Kurs Positionen
  foreach my $pos (sort(keys(%{$self->{Kurs}}))) {
    my $kurs = $self->{Kurs}->{$pos};
    if ($kurs->{Basiswert}) {
      if ($kurs->{Basiswert} !~ /$kurs->{Name}$/) {
        $kurs->{Basiswert} = $kurs->{Basiswert} . '/' . $kurs->{Name};
        $kurs->{Name} = $kurs->{Basiswert};
      }
    } else {
      $kurs->{Basiswert} = $kurs->{Name};
    }
    next if $kurs->{exchange};
    Trace->Trc('S', 1, 0x00001, "Umrechnung Position: $pos");
    if ($kurs->{Currency} ne $self->{BasisCur}) {
      if (my $rate = $self->{Kurs}->{$kurs->{Currency} . $self->{BasisCur}}->{Price}) {
        foreach my $attribute ('Change', 'Change_Day', 'Price') {
          if ($kurs->{$attribute}) {
            $kurs->{$attribute} *= $rate;
          }
        }
        $kurs->{Currency} = $self->{BasisCur};
      }
    }
    if (defined($kurs->{Dividend_Currency}) && ($kurs->{Dividend_Currency} ne $self->{BasisCur})) {
      if (my $rate = $self->{Kurs}->{$kurs->{Dividend_Currency} . $self->{BasisCur}}->{Price}) {
        foreach my $attribute ('Dividend') {
          if ($kurs->{$attribute}) {
            $kurs->{$attribute} *= $rate;
          }
        }
        $kurs->{Dividend_Currency} = $self->{BasisCur};
      }
    }

    # Faktorisieren der Kurse falls konfiguriert
    foreach my $attribute (keys(%{$kurs})) {
      if ($self->{Flags}{$attribute}{Faktor}) {
        $kurs->{$attribute} *= $self->{Flags}{$attribute}{Faktor};
      }
    }

    # Speichern der High-/Low-Werte
    $kurs->{Price_High}      = defined($HighLow{"H_$pos"})  ? $HighLow{"H_$pos"}  : 0;
    $kurs->{Price_High_Date} = defined($HighLow{"HD_$pos"}) ? $HighLow{"HD_$pos"} : time2str('%d.%m.%y', time());
    $kurs->{Price_Low}       = defined($HighLow{"L_$pos"})  ? $HighLow{"L_$pos"}  : 999999;
    $kurs->{Price_Low_Date}  = defined($HighLow{"LD_$pos"}) ? $HighLow{"LD_$pos"} : time2str('%d.%m.%y', time());
    if ($kurs->{Price}) {
      my $newHigh = ($kurs->{Price} > $kurs->{Price_High});
      if (!defined($reset{"H_$pos"})) {
        $newHigh = $newHigh && (($kurs->{Price} < 2*$kurs->{Price_High}) || ($kurs->{Price_High} <= 0));
      }
      if ($newHigh) {
        $HighLow{"H_$pos"}  = $kurs->{Price_High}      = $kurs->{Price};
        $HighLow{"HD_$pos"} = $kurs->{Price_High_Date} = time2str('%d.%m.%y', time());
      }
      my $newLow  = ($kurs->{Price} < $kurs->{Price_Low});
      if (!defined($reset{"L_$pos"})) {
        $newLow = $newLow && (($kurs->{Price} > 0.5*$kurs->{Price_Low}) || ($kurs->{Price_Low} >999998));
      }
      if ($newLow) {
        $HighLow{"L_$pos"}  = $kurs->{Price_Low}      = $kurs->{Price};
        $HighLow{"LD_$pos"} = $kurs->{Price_Low_Date} = time2str('%d.%m.%y', time());
      }
      foreach my $resetpos (keys(%reset)) {
        my $regex = eval { qr/$resetpos/ };
        next if $@;
        if ($pos =~ /$resetpos/) {
          $HighLow{"L_$pos"}  = $HighLow{"H_$pos"}  = $kurs->{Price};
          $HighLow{"LD_$pos"} = $HighLow{"HD_$pos"} = time2str('%d.%m.%y', time());
        }
      }
    }
  }
  if (Configuration->config('Prg', 'HighLow')) {
    my $HighLowRef = \%HighLow;
    my $hlFile = Utils::extendString(Configuration->config('Prg', 'HighLow'), "BIN|$Bin|SCRIPT|" . uc($Script));
    store($HighLowRef, $hlFile);
  }

  # Umrechnen aller Cash Positionen
  foreach my $owner (keys(%{$self->{Cash}})) {
    foreach my $bank (keys %{$self->{Cash}{$owner}}) {
      foreach my $cur (keys %{$self->{Cash}{$owner}{$bank}}) {
        next if $cur eq $self->{BasisCur};
        if (my $rate = $self->{Kurs}->{$cur . $self->{BasisCur}}->{Price}) {
          $self->{Cash}{$owner}{$bank}{$cur} *= $rate;
        }
      }
    }
  }

  # Ab hier ist alles in Basiswaehrung umgerechnet

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Kurse_umrechnen


sub Portofolios_summieren {
  #################################################################
  # Summiert identische Einzelpositionen auf und ergaenzt die Werte
  # (verfuegbar bei: P-Portofolio, W-Watchlist, S-Summe)
  # PW  Name                : Name der Position
  # PW  WKN                 : WKN der Position
  # PW  Symbol              : Kurzsymbol der Position
  # PW  Symbol_Alternatives : Symbol der Position mit Handelsplatz
  # PW  Branche             : Branche
  # PW  Sector              : Sector
  # PW  Quantity            : Amount of Shares in Position
  # PWS Weight_Dep          : Weighth of the Position in the Portofolio
  # PW  Price               : Aktueller Preis per Share
  # PW  Price_Low           : Niedrigerster Preis per Share seit Kauf
  # PW  Price_Low_Date      : Datum des niedrigersten Preises per Share seit Kauf
  # PW  Price_High          : Hoechster Preis per Share seit Kauf
  # PW  Price_High_Date     : Datum des hoechsten Preises per Share seit Kauf
  # PWS Price_Pos           : Aktueller Preis der Position ED-Head/S-Data
  # PW  Price_Last          : Letzter Preis per Share
  # PWS Price_Last_Pos      : Letzter Preis der Position ED-Head/S-Data
  # PW  Price_Buy           : Kaufpreis per Share
  # PWS Price_Buy_Pos       : Kaufpreis der Position ED-Head/S-Data
  # PW  Change              : Absolute Veraenderung per Share seit Kauf
  # PWS Change_Pos          : Absolute Veraenderung der Position seit Kauf ED-Head/S-Data
  # PWS Change_Percent      : Prozentuale Veraenderung seit Kauf ED-Head/S-Data
  # PWS Change_Percent_Dep  : Prozentuale Aenderung des Portofoilis seit Kauf bezogen auf den aktuellen Kurs des gesamten Portofolios ED-Head/S-Data
  # PW  Change_Percent_High : Prozentuale Veraenderung des hoechsten Preises per Share seit Kauf
  # PW  Change_Percent_Low  : Prozentuale Veraenderung des niedrigsten Preises per Share seit Kauf
  # PW  Change_Day          : Absolute Veraenderung per Share am aktuellen Tag
  # PWS Change_Day_Pos      : Absolute Veraenderung der Position am aktuellen Tag
  # PWS Change_Day_Percent  : Prozentuale Veraenderung am aktuellen Tag
  # PW  Currency            : Waehrung der Position
  # PW  Dividend            : Dividende per Share
  # PWS Dividend_Pos        : Absolute Dividende in Position
  # PW  Dividend_Date       : Datum der Dividendenausschuettung
  # PW  Dividend_Days       : Tage bis zur Dividendenausschuettung
  # PW  Dividend_Weeks      : Wochen bis zur Dividendenausschuettung
  # PWS Dividend_Yield      : Prozentuale Dividende bezogen auf den aktuellen Wert
  # PW  Dividend_Currency   : Waehrung der Dividendenausschuettung
  # PW  Last_Trade          : Zeitpunkt des letzter Handels
  # PW  Last_Trade_Date     : Datum des letzter Handels
  # PW  Last_Trade_Time     : Uhrzeit des letzter Handels
  # PW  Last_Trade_TS       : Timestamp des letzter Handels
  # PW  Last_Trade_Days_ago : Wieviele Tage liegt der letzte Handel zurück
  # PW  letzter_Abruf       : Timestamp der letzten Kursermittelung
  # PW  Stock_Exchange      : Handelsplatz
  # PW  aktuell             : Flag, das anzeigt, ob der Kurs aktuell ist

  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  POSIX::setlocale(&POSIX::LC_ALL, 'de_DE');

  $self->{Portofolios}{Summe} = {};
  foreach my $PFName (keys(%{$self->{Portofolios}})) {
    if ($PFName ne 'Summe') {
      if ($self->{SumPos}) {
        my %PFHash;
        foreach my $pos (keys %{$self->{Portofolios}{$PFName}}) {
          my ($symbol, $count) = split(/ /, $pos);
          if ($symbol) {
            # Aufsummieren aller Einzelpositionen in eine Position
            if (!defined($PFHash{"$symbol 1"})) {
              $PFHash{"$symbol 1"} = $self->{Portofolios}{$PFName}{$pos};
            } else {
              foreach my $attribute (keys %{$self->{Portofolios}{$PFName}{$pos}}) {
                if (($attribute eq 'Quantity') || ($attribute =~ /_Pos$/)) {
                  $PFHash{"$symbol 1"}{$attribute} += $self->{Portofolios}{$PFName}{$pos}{$attribute};
                }
              }
            }
          }
        }
        $self->{Portofolios}{$PFName} = \%PFHash;
      }

      $self->{Portofolios}{$PFName}{Summe} = {};
      foreach my $pos (keys %{$self->{Portofolios}{$PFName}}) {
        next if ($pos eq 'Summe');
        # Aufsummieren aller Depotwerte in eine Depotsummenposition
        my $posptr  = $self->{Portofolios}{$PFName}{$pos};
        my @symbols = split(/\|/, $posptr->{Symbol_Alternatives});
        my %kurs;
        foreach (@symbols) {
          next if %kurs;
          if ($self->{Kurs}{$_}{aktuell}) {
            %kurs = %{$self->{Kurs}{$_}};
            $posptr->{Symbol_Local} = $_;
          }
        }

        # Ergaenzen der Positionsinfos
        $posptr->{Name}                = $kurs{Name} if defined($kurs{Name});
        if (looks_like_number($posptr->{Quantity}) && $posptr->{Quantity} && looks_like_number($posptr->{Price_Buy_Pos})) {
          $posptr->{Price_Buy} = $posptr->{Price_Buy_Pos} / $posptr->{Quantity};
        } else {
          Trace->Trc('I', 2, 0x0220f, $PFName, $pos, $posptr->{Quantity}, looks_like_number($posptr->{Quantity}), $posptr->{Price_Buy_Pos}, looks_like_number($posptr->{Price_Buy_Pos}));
          $posptr->{Price_Buy} = '0';
        }
        $posptr->{Price}               = $kurs{Price} ? $kurs{Price} : $posptr->{Price_Buy};
        $posptr->{Price_Low}           = $kurs{Price_Low} ? $kurs{Price_Low} : $posptr->{Price};
        $posptr->{Price_Low_Date}      = defined($kurs{Price_Low_Date}) ? $kurs{Price_Low_Date} : '';
        $posptr->{Price_High}          = $kurs{Price_High} ? $kurs{Price_High} : $posptr->{Price};
        $posptr->{Price_High_Date}     = defined($kurs{Price_High_Date}) ? $kurs{Price_High_Date} : '';
        $posptr->{Price_Pos}           = $posptr->{Quantity} * $posptr->{Price};
        $posptr->{Change}              = $posptr->{Price} - $posptr->{Price_Buy};
        $posptr->{Change_Pos}          = $posptr->{Price_Pos} - $posptr->{Price_Buy_Pos};
        $posptr->{Change_Day}          = $kurs{Change_Day} ? $kurs{Change_Day} : 0;
        $posptr->{Change_Day_Pos}      = $kurs{Change_Day} ? $posptr->{Quantity} * $kurs{Change_Day} : 0;
        $posptr->{Change_Day_Percent}  = $kurs{Change_Day_Percent} ? $kurs{Change_Day_Percent} : 0;
        $posptr->{Price_Last}          = $posptr->{Price} - $posptr->{Change_Day};
        $posptr->{Price_Last_Pos}      = $posptr->{Price_Pos} - $posptr->{Change_Day_Pos};
        $posptr->{Last_Trade_Days_ago} = defined($kurs{Last_Trade_Days_ago}) ? $kurs{Last_Trade_Days_ago} : '';
        $posptr->{Last_Trade_Date}     = defined($kurs{Last_Trade_Date}) ? $kurs{Last_Trade_Date} : '';
        $posptr->{Last_Trade_Time}     = defined($kurs{Last_Trade_Time}) ? $kurs{Last_Trade_Time} : '';
        $posptr->{Last_Trade}          = defined($kurs{Last_Trade}) ? $kurs{Last_Trade} : '';
        $posptr->{Last_Trade_TS}       = defined($kurs{Last_Trade_TS}) ? $kurs{Last_Trade_TS} : '';

        $posptr->{letzter_Abruf}       = defined($kurs{letzter_Abruf}) ? $kurs{letzter_Abruf} : '';
        $posptr->{aktuell}             = defined($kurs{aktuell}) ? $kurs{aktuell} : 0;

        $posptr->{Stock_Exchange}      = defined($kurs{Stock_Exchange}) ? $kurs{Stock_Exchange} : '';

        # Dividendeninfos falls vorhanden aus Kurs holen andernfalls aus ini
#        $posptr->{Dividend}            = $kurs{Dividend} if (defined($kurs{Dividend}) && (!defined($posptr->{Dividend}) || ($posptr->{Dividend} eq '')));
        $posptr->{Dividend}            = $kurs{Dividend} if defined($kurs{Dividend});
        $posptr->{Dividend}            ||= 0;
        $posptr->{Dividend_Pos}        = $posptr->{Quantity} * $posptr->{Dividend};
        if ($posptr->{Dividend} > 0) {
          $posptr->{Dividend_Date}     = $kurs{Dividend_Date} if (defined($kurs{Dividend_Date}) && (!defined($posptr->{Dividend_Date}) || ($posptr->{Dividend_Date} eq '')));
          $posptr->{Dividend_Date}     = "01/01/2000" if !$posptr->{Dividend_Date};
          if ($posptr->{Dividend_Date} =~ m/([0-9]+)\.([0-9]+)\.([0-9]+)$/) {
            $posptr->{Dividend_Date}   = "$2/$1/$3";
          }
          $posptr->{Dividend_Date_TS}  = str2time($posptr->{Dividend_Date}) || 0;
          $posptr->{Dividend_Date}     = time2str('%d.%m.%y', $posptr->{Dividend_Date_TS});
          $posptr->{Dividend_Days}     = $posptr->{Dividend_Date_TS} - time();
          $posptr->{Dividend_Days}     = ($posptr->{Dividend_Date_TS} - time)/86400 + 3650;
          if ($posptr->{Dividend_Days} >= 0) {
            $posptr->{Dividend_Days}   %= 365;
            $posptr->{Dividend_Weeks}  = int($posptr->{Dividend_Days}/7);
          } else {
            $posptr->{Dividend_Days}   = '';
            $posptr->{Dividend_Weeks}  = '';
            $posptr->{Dividend_Date}   = '';
          }
        } else {
          $posptr->{Dividend_Date}     = '';
          $posptr->{Dividend_Date_TS}  = 0;
          $posptr->{Dividend_Days}     = '';
          $posptr->{Dividend_Weeks}    = '';
          $posptr->{Dividend_Date}     = '';
        }

        # Aufsummieren der Werte fuer die Gesamtsumme pro Portofolio
        $self->{Portofolios}{$PFName}{Summe}{Price_Buy_Pos}  += $posptr->{Price_Buy_Pos};
        $self->{Portofolios}{$PFName}{Summe}{Price_Last_Pos} += $posptr->{Price_Last_Pos};
        $self->{Portofolios}{$PFName}{Summe}{Price_Pos}      += $posptr->{Price_Pos};
        $self->{Portofolios}{$PFName}{Summe}{Change_Day_Pos} += $posptr->{Change_Day_Pos};
        $self->{Portofolios}{$PFName}{Summe}{Change_Pos}     += $posptr->{Change_Pos};
        $self->{Portofolios}{$PFName}{Summe}{Dividend_Pos}   += $posptr->{Dividend_Pos} ;

        # Aufsummieren der Werte fuer die Uebersicht der Portofolios
        $self->{Portofolios}{Summe}{$PFName}{Price_Buy_Pos}  += $posptr->{Price_Buy_Pos};
        $self->{Portofolios}{Summe}{$PFName}{Price_Last_Pos} += $posptr->{Price_Last_Pos};
        $self->{Portofolios}{Summe}{$PFName}{Price_Pos}      += $posptr->{Price_Pos};
        $self->{Portofolios}{Summe}{$PFName}{Change_Day_Pos} += $posptr->{Change_Day_Pos};
        $self->{Portofolios}{Summe}{$PFName}{Change_Pos}     += $posptr->{Change_Pos};
        $self->{Portofolios}{Summe}{$PFName}{Dividend_Pos}   += $posptr->{Dividend_Pos} ;

        # Aufsummieren der Werte fuer die Gesamtsumme der Uebersicht der Portofolios
        # Falls es keine Watchlist ist
        if ($PFName !~/^Watchlist /) {
          $self->{Portofolios}{Summe}{Summe}{Price_Buy_Pos}  += $posptr->{Price_Buy_Pos};
          $self->{Portofolios}{Summe}{Summe}{Price_Last_Pos} += $posptr->{Price_Last_Pos};
          $self->{Portofolios}{Summe}{Summe}{Price_Pos}      += $posptr->{Price_Pos};
          $self->{Portofolios}{Summe}{Summe}{Change_Day_Pos} += $posptr->{Change_Day_Pos};
          $self->{Portofolios}{Summe}{Summe}{Change_Pos}     += $posptr->{Change_Pos};
          $self->{Portofolios}{Summe}{Summe}{Dividend_Pos}   += $posptr->{Dividend_Pos} ;
        }
      }
    }
    $self->{Portofolios}{$PFName}{Summe}{Dividend_Currency} = $self->{BasisCur};
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Portofolios_summieren


sub Portofolios_analysieren {
  #################################################################
  # Ermittelt Kennzahlen zu den Portofolios
  #
  # PW  Rating              : Bewertung des Basiswertes
  # PWS Weight_Dep          : Anteil der Position am Depot
  #

  #
  # Prozedurnummer 4
  #
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;

  sub rating {
    my $value  = shift || 0;
    my $mode   = shift || 'asc';
    my $red    = shift || 0;
    my $yellow = shift || 0;
    my $blue   = shift || 0;
    my $green  = shift || 0;

    if ($mode eq "asc") {  # i.E.: mm -10, m -5, n 5, p 10 pp
      return 'mm'      if $value <= $red;
      return 'm'       if $value <= $yellow;
      return 'neutral' if $value < $blue;
      return 'p'       if $value < $green;
      return 'pp';
    }

    if ($mode eq "desc") {  # i.E.: mm 3, m 2, n 1, p 0 pp
      return 'mm'      if $value >= $red;
      return 'm'       if $value >= $yellow;
      return 'neutral' if $value > $blue;
      return 'p'       if $value > $green;
      return 'pp';
    }

    if ($mode eq 'range') {  # i.E.: mm 1, n 3, pp 6, n 10 mm
      return 'mm'      if $value <= $red;
      return 'mm'      if $value >= $green;
      return 'neutral' if $value <= $yellow;
      return 'neutral' if $value >= $blue;
      return 'pp';
    }
  }

  my $rc = 0;

  POSIX::setlocale(&POSIX::LC_ALL, 'de_DE');

  foreach my $depot (keys %{$self->{Portofolios}}) {
    foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
      my $posptr  = $self->{Portofolios}{$depot}{$pos};
      my $kursptr = defined($posptr->{Symbol_Local}) ? $self->{Kurs}{$posptr->{Symbol_Local}} : undef;
      # Prozentuale Werte und Gewichtung berechnen
#      if (defined($kursptr->{Dividend_Yield})) {
#        $posptr->{Dividend_Yield}       = $kursptr->{Dividend_Yield};
#      } else {
#        if ($posptr->{Price}) {
#          $posptr->{Dividend_Yield}     = 100 * $posptr->{Dividend} / $posptr->{Price};
#        } else {
#          if ($posptr->{Price_Pos}) {
#            $posptr->{Dividend_Yield}   = 100 * $posptr->{Dividend_Pos} / $posptr->{Price_Pos};
#          } else {
#            $posptr->{Dividend_Yield}   = 0;
#          }
#        }
#      }
      $posptr->{Price} = 0 if !$posptr->{Price};
      if ($posptr->{Price} > 0) {
        $posptr->{Dividend_Yield}     = 100 * $posptr->{Dividend} / $posptr->{Price};
      } else {
        $posptr->{Price_Pos} = 0 if !$posptr->{Price_Pos};
        if ($posptr->{Price_Pos} > 0) {
          $posptr->{Dividend_Yield}   = 100 * $posptr->{Dividend_Pos} / $posptr->{Price_Pos};
        } else {
          if (defined($kursptr->{Dividend_Yield})) {
            $posptr->{Dividend_Yield} = $kursptr->{Dividend_Yield};
          } else {
            $posptr->{Dividend_Yield}   = 0;
          }
        }
      }
      $posptr->{Dividend_Currency}      = $self->{BasisCur};
      $posptr->{Change_Percent}         = $posptr->{Price_Buy_Pos} ? 100 * $posptr->{Change_Pos} / $posptr->{Price_Buy_Pos} : 0;
      $posptr->{Change_Percent_Low}     = $posptr->{Price_Low} ? 100 * ($posptr->{Price}-$posptr->{Price_Low}) / $posptr->{Price_Low} : 0;
      $posptr->{Change_Percent_High}    = $posptr->{Price_High} ? 100 * ($posptr->{Price_High}-$posptr->{Price}) / $posptr->{Price_High} : 0;
      if (!defined($posptr->{Change_Day_Percent})) {
        $posptr->{Change_Day_Percent}   = $posptr->{Price_Last_Pos} ? 100 * $posptr->{Change_Day_Pos} / $posptr->{Price_Last_Pos} : 0;
      }
      $posptr->{Change_Day_Percent}     =~ s/%$// if (defined($posptr->{Change_Day_Percent}));
      $posptr->{Weight_Dep}             = $self->{Portofolios}{$depot}{Summe}{Price_Pos} ? 100  * $posptr->{Price_Pos} / $self->{Portofolios}{$depot}{Summe}{Price_Pos} : 0;
      $posptr->{Change_Percent_Dep}     = $self->{Portofolios}{$depot}{Summe}{Price_Pos} ? 1000 * $posptr->{Change_Pos} / $self->{Portofolios}{$depot}{Summe}{Price_Pos} : 0;
      $posptr->{Change_Day_Percent_Dep} = $self->{Portofolios}{$depot}{Summe}{Price_Pos} ? 1000 * $posptr->{Change_Day_Pos} / $self->{Portofolios}{$depot}{Summe}{Price_Pos} : 0;

      # Position bewerten
      foreach my $attribute (keys %{$posptr}) {
        if (my $range = Configuration->config("Rating", $attribute)) {
          $posptr->{Rating}{$attribute} = color($self->{Color}->{rating($posptr->{$attribute}, split(' ', $range))});
        }
      }
    }
  }

  foreach my $symbol (keys %{$self->{Kurs}}) {
    foreach my $flagname (keys %{$self->{Kurs}{$symbol}}) {
      if ($self->{Flags}{$flagname}{Format}) {
        # @@@
        $self->{Kurs}{$symbol}{$flagname} = 0 if ($flagname =~ /^p_/ && $self->{Kurs}{$symbol}{$flagname} =~ /%$/);
        Trace->Trc('I', 5, 0x02210, $symbol, $flagname, $self->{Kurs}{$symbol}{$flagname}, $self->{Flags}{$flagname}{Format});
      }
    }
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Portofolios_analysieren


sub createFormatStrg {
  #################################################################
  # Erzeuge den Ausgabeformatierstring
  #

  # Prozedurnummer 5
  #

  my $self = shift;

  my %args = ('Type'       => '',
              'Depot'      => '',
              'Position'   => '',
              'Attribute'  => '',
              @_);

  my $type    = $args{'Type'};
  my $depot   = $args{'Depot'};
  my $pos     = $args{'Position'};
  my $att     = $args{'Attribute'};
  my $bg      = $args{'Background'};

  my %formatHash;
  # Konsolensteuerung (Farbe):
  # Shortcuts
  $formatHash{C_C} = color($self->{Color}->{'clear'});
  $formatHash{R_C} = color($self->{Color}->{'reset'});
  $formatHash{N_C} = color($self->{Color}->{'neutral'});
  $formatHash{I_C} = color($self->{Color}->{'invers'});
  $formatHash{B_C} = color($bg) if $bg;

  # Langversion
  foreach (keys(%{$self->{Color}})) {$formatHash{$_} = color($self->{Color}->{$_})}

  if ($type eq "Portofolios") {
    # Aufnahme individueller Werte aus der Position
    if (defined(my $posptr = $self->{Portofolios}{$depot}{$pos})) {
      $formatHash{Depot}    = $depot;
      $formatHash{Position} = $pos;
      foreach my $flag (keys %{$posptr}) {
        my $value = $posptr->{$flag};
        if (defined($value) && !ref($value) && !defined($formatHash{$flag})) {
          $formatHash{$flag} = $value;
        }
      }

      # Aufnahme allgemeiner Werte aus der Kursinfo
      my $localSymbol = $self->{Portofolios}{$depot}{$pos}{Symbol_Local};
      if (defined($localSymbol) && defined($self->{Kurs}{$localSymbol})) {
        foreach my $flag (keys %{$self->{Kurs}{$localSymbol}}) {
          if (!defined($formatHash{$flag})) {
            $formatHash{$flag} = $self->{Kurs}{$localSymbol}{$flag};
          }
        }
      }

      # Colorierung der Rating Werte
      foreach my $attribute (keys %{$self->{Portofolios}{$depot}{$pos}{Rating}}) {
        $formatHash{"${attribute}_C"} = $self->{Portofolios}{$depot}{$pos}{Rating}{$attribute};
      }
    }
  }

  # Cashdefinitionen
  if ($type eq "Cash") {
    if (defined($self->{Cash}{$depot}{$pos}{$att})) {
      $formatHash{Owner}            = $depot;
      $formatHash{Bank}             = $pos;
      $formatHash{Currency_Account} = $att;
      $formatHash{Balance}          = $self->{Cash}{$depot}{$pos}{$att};
    }
  }

  # Ersetzen der '|' durch '/'
  foreach (keys(%formatHash)) {
    $formatHash{$_} =~s/\|/\//g;
  }
  $self->{Formathash} = \%formatHash;

  return join('|', %formatHash);
} ## end sub createFormatStrg


sub Ausgabe_schreiben {
  #################################################################
  # Schreiben der Ausgabedatei(en)
  # Ergebnis:
  #

  # Prozedurnummer 5
  #

  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;

  my $rc = 0;

  # Abarbeitung aller Portofolio- und Cash- Eintraege
  my %ausgabedateityp = Configuration->config('Ausgabedateityp');
  foreach my $depot (keys %{$self->{Portofolios}}) {
    my ($depottyp, $depotname);
    if ($depot =~ /^([\S]*)(\s(.+))?/) {
      $depottyp  = $1;
      $depotname = $3;

      Trace->Trc('I', 2, "Bearbeite Depot <$depot>.");
    
      if (my $ausgabedatei = Configuration->config('Ausgabedateityp', $depottyp)) {
        my $ausgabename   = Utils::extendString($ausgabedatei, "Depot|$depot", '');
        my $exportname    = Utils::extendString(Configuration->config('Ausgabedateityp', 'Export'), "Depot|$depot", '');
        if (my $ausgabeformat = Configuration->config("Ausgabeformat_$depottyp")) {
          $self->{Ausgabe}{$ausgabename}{Color} = $ausgabeformat->{'Color'};
          # Fuer alle Dateien
          if (defined(my $head = $ausgabeformat->{'Head'})) {
            Trace->Trc('I', 5, "  Bearbeite Ausgabedatei <$depottyp> Head <$head>.");
            # Erzeuge Formatstring fuer Head
            my $formatstr = $self->createFormatStrg('Type',     'Portofolios',
                                                    'Depot',    $depot,
                                                    'Position', 'Summe');
            $formatstr    =~ s/([^\|]*_C\|)[^\|]*/$1/g if !$ausgabeformat->{'Color'};
            my $seperator = $ausgabeformat->{'Sep'};
            my $colHead   = $ausgabeformat->{'Col'};
            # Wenn Head dann erzeuge und Schreiben Headstring
            push(@{$self->{Ausgabe}{$ausgabename}{Head}}, Utils::extendString($head, $formatstr, ''));
            push(@{$self->{Ausgabe}{$ausgabename}{Head}}, Utils::extendString($seperator, $formatstr, '')) if (defined($seperator));
            push(@{$self->{Ausgabe}{$ausgabename}{Head}}, Utils::extendString($colHead, $formatstr, ''))   if (defined($colHead));
            push(@{$self->{Ausgabe}{$ausgabename}{Head}}, Utils::extendString($seperator, $formatstr, '')) if (defined($seperator));
          }
    
          my $data   = $ausgabeformat->{'Data'};
          my $export = $ausgabeformat->{'Export'};
          if ($data || $export) {
            foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
              # Fuer alle Positionen <> Summe
              next if ($pos eq 'Summe');
              Trace->Trc('I', 5, "  Bearbeite Ausgabedatei <$depottyp> Position <$pos>");
              # Erzeuge Formatstring fuer Zeile
              my $formatstr = $self->createFormatStrg('Type',     'Portofolios',
                                                      'Depot',    $depot,
                                                      'Position', $pos);
              $formatstr    =~ s/([^\|]*_C\|)[^\|]*/$1/g if !$ausgabeformat->{'Color'};
              my ($posname, $cntposname);
              if (defined($self->{Portofolios}{$depot}{$pos}{Name}) && $self->{Portofolios}{$depot}{$pos}{Name} ne '') {
                $posname = uc($self->{Portofolios}{$depot}{$pos}{Name})
              } else {
                $posname = uc($pos);
              }
              $cntposname = $posname;
              # Fuer alle Dateien
              # Wenn Data dann erzeuge und Schreiben Datastring
              my $count = 1;
              if ($data) {
                while (defined($self->{Ausgabe}{$ausgabename}{Data}{$posname})) {
                  $posname = uc("$cntposname $count");
                  $count++;
                }
                $self->{Ausgabe}{$ausgabename}{Data}{$posname} =  Utils::extendString($data, $formatstr, '') if $data;
              }
              if ($export) {
                while (defined($self->{Ausgabe}{$exportname}{Data}{$posname})) {
                  $posname = uc("$cntposname $count");
                  $count++;
                }
                $self->{Ausgabe}{$exportname}{Data}{$posname} =  Utils::extendString($export, $formatstr, '') if $export;
              }
            }
          }
        }
      }
    }
  }

  # Abarbeitung der Cashliste
  my $count = 0;
  foreach my $owner (sort keys %{$self->{Cash}}) {
    foreach my $bank (sort keys %{$self->{Cash}{$owner}}) {
      next if ($bank eq 'Summe');
      foreach my $currency (sort keys %{$self->{Cash}{$owner}{$bank}}) {
        my $formatstr = $self->createFormatStrg('Type',      'Cash',
                                                'Depot',     $owner,
                                                'Position',  $bank,
                                                'Attribute', $currency);
        my $name = Utils::extendString($ausgabedateityp{'Cash'}, $formatstr, '');
        if (defined(my $head = Configuration->config("Ausgabeformat_Cash", 'Head'))) {
          push(@{$self->{Ausgabe}{$name}{Head}}, Utils::extendString($head, $formatstr, ''));
        }
        if (defined(my $data = Configuration->config("Ausgabeformat_Cash", 'Data'))) {
          $self->{Ausgabe}{$name}{Data}{$count++} = Utils::extendString($data, $formatstr, '');
        }
      }
    }
  }

  # Ausgabedateien schreiben
  my %tmpdatei;
  foreach my $name (keys %{$self->{Ausgabe}}) {
    next if !defined($self->{Ausgabe}{$name}{Data});
    Trace->Trc('I', 2, 0x02501, $name);
    eval {mkpath(dirname($name));};
    $tmpdatei{$name} = $name . ".$$";
    open(DEPOT, '>> ' . $tmpdatei{$name});
    if ($name =~ /csv$/) {
      binmode DEPOT, ':encoding(cp1252)';
    } else {
      binmode DEPOT, ':encoding(utf8)';
    }
    while (my $line = shift(@{$self->{Ausgabe}{$name}{Head}})) {
      if ($self->{Ausgabe}{$name}{Color}) {
        $line = $line . color($self->{Color}->{reset});
      }
      print DEPOT "$line\n";
    }
    # Data alphabetisch sortiert ausgeben
    my $bg = $self->{Color}->{normal};
    foreach my $posname (sort keys %{$self->{Ausgabe}{$name}{Data}}) {
      my $line = $self->{Ausgabe}{$name}{Data}{$posname};
      if ($self->{Ausgabe}{$name}{Color}) {
        $line = color($bg) . $line . color($self->{Color}->{reset});
        $bg = $bg eq $self->{Color}->{normal} ? $self->{Color}->{invers} : $self->{Color}->{normal};
      }
      print DEPOT "$line\n";
    }
    close(DEPOT);
  }

  # Atomares Erzeugen der Zieldateien
  foreach (keys %tmpdatei) {move($tmpdatei{$_}, $_)}

  # Kursinfos löschen
  if (exists($self->{VarFile})) {
    eval {unlink($self->{VarFile});};
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Ausgabe_schreiben


1;
