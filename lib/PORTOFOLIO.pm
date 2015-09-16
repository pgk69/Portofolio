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

# use utf8;      # so literals and identifiers can be in UTF-8
# use v5.12;     # or later to get "unicode_strings" feature
# use strict;    # quote strings, declare variables
# use warnings;  # on by default
# use warnings  qw(FATAL utf8);    # fatalize encoding glitches
use open qw(:utf8 :std);    # undeclared streams in UTF-8

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
use Term::ANSIColor;
use File::Copy;
use File::Path;
use File::Basename;
use FindBin qw($Bin $Script $RealBin $RealScript);
use Storable;
use Finance::Quote;

#
# Konstantendefinition
#

#
# Variablendefinition
#

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
  if (Configuration->config('Stockservice', 'Source') eq 'YAHOO') {
    Trace->Exit(1, 0, 0x0f009, 'Stockservice', 'URL') if (!defined(Configuration->config('Stockservice', 'URL')));
    Trace->Exit(1, 0, 0x0f009, 'Stockinfos') if (!defined(Configuration->config('Stockinfos')));
  }

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
  }

  # Positionen aufsummieren
  $self->{SumPos} = Configuration->config('Prg', 'PosAufsummieren') ? 1 : 0;

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

  # Aufbau der Abfrage-URL
  my %stockInfos = Configuration->config('Stockinfos');
  my %flags;

  foreach (keys %stockInfos) {
    my ($name, $info, $faktor, $laenge, $default, @format) = split(' ', $stockInfos{$_});
    if (!defined($default) || ($default eq '-')) {$default = ''}
    $flags{$_} = {Abbr    => $name,
                  Flag    => $info,
                  Faktor  => $faktor || 0,
                  Laenge  => $laenge,
                  Default => $default,
                  Format  => join(' ', @format) || '%s'};
  }

  $self->{Flags} = \%flags;

  if (my $Testkurs = Configuration->config('Stockservice', 'Testkurs')) {
    $self->{Testkurs} = $Testkurs;
  }

  # Maximales Alter von Kursinfos
  $self->{MaxDelay} = Configuration->config('Stockservice', 'MaxDelay') || 86400;

  # Differenz uzu UTC
  $self->{UTCDelta} = Configuration->config('Stockservice', 'UTCDelte') || +0100;

  # Farben
  $self->{BG}->{normal}  = ' ' . (Configuration->config('Color', 'normal')  || 'on_black');
  $self->{BG}->{invers}  = ' ' . (Configuration->config('Color', 'invers')  || 'on_blue');
  $self->{FG}->{neutral} =        Configuration->config('Color', 'neutral') || 'clear ';
  $self->{FG}->{pp}      =        Configuration->config('Color', 'pp')      || 'green ';
  $self->{FG}->{p}       =        Configuration->config('Color', 'p')       || 'cyan ';
  $self->{FG}->{m}       =        Configuration->config('Color', 'm')       || 'magenta ';
  $self->{FG}->{mm}      =        Configuration->config('Color', 'mm')      || 'red ';

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


sub _webabruf_YAHOO {
  #################################################################
  # Durchfuehren eines Webabrufs

  #
  # Prozedurnummer 1
  #
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 3, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $symbol  = shift;
  my $hashptr = shift;
  my $flagptr = shift;

  no autovivification;

  my $rc = 0;

  if (!$$hashptr{_aktuell} && (!$$hashptr{_letzter_Abruf} || ($$hashptr{_letzter_Abruf} < time - $self->{MaxDelay}))) {
    push(@$flagptr, ('last', 'date', 'time'));
    my %unique = ();
    $unique{$_} = 0 for @$flagptr;
    my @flags = sort(keys %unique);

    Trace->Trc('I', 2, 0x02101, $symbol, join('', @flags));

    my (@myFlags, $flag, $flagname, $stockdata, $wert);

    my $webAbruf = Utils::extendString(Configuration->config('Stockservice', 'URL'), 'STOCK|' . $symbol . '|DATA|' . join('', @flags));
    my $maxtry = Configuration->config('Stockservice', 'Anzahl_Versuche') || 10;

    my $crlfmerker = $/;
    $/ = '%';

    $$hashptr{_aktuell} = 0;
    do {
      @myFlags   = @flags;
      $stockdata = get($webAbruf);
      if (defined($stockdata) && $stockdata !~ 'Missing') {

        # die "Failed to get Stock Data" unless defined $stockdata;
        while ($stockdata =~ /\"[\s0]*([^\"]+?)[\s]*\"|[\s0]*([^,\s]+)/g) {
          chomp($wert = $1 ? $1 : $2);
          $wert = 0 if ($wert eq 'N/A');
#          $flag = shift(@myFlags);
          $flagname = shift(@myFlags);
          $flag     = $self->{Flags}{$flagname}{Abbr};
          if (defined($flag) && $flag ne '-') {
            # Sichern des Originalwertes
            if ($self->{Flags}{$flagname}{Faktor}) {$$hashptr{$flagname . '_RAW'} = $wert}
            if ($self->{Flags}{$flagname}{Faktor} && $$hashptr{_Waehrung}) {
              if ($self->{Flags}{$flagname}{Faktor} == 1) {
                my $curxchg = 'EUR' . $$hashptr{_Waehrung} . '=X';
                # Falls der Wechselkurs fuer die Waehrung noch nicht ermittelt ist,
                # versuchen wir das
                if (!$self->{Kurs}{$curxchg}{last}) {
                  # Default -> Wechselkurs 1
                  $self->{Kurs}{$curxchg}{last} = 1;
                  if ($$hashptr{_Waehrung} ne 'EUR') {
                    if ($$hashptr{_Waehrung} =~ /^[0-9.,]+/) {
                      # Keine Standardwaehrung -> Wechselkurs wird direkt angegeben
                      $self->{Kurs}{$curxchg}{last} = $$hashptr{_Waehrung};
                    } elsif ($$hashptr{_Waehrung} =~ /^[A-Za-z][A-Za-z0-9]{2}/) {
                      # Standardwaehrung -> Wechselkurs wird ermittelt
                      $self->_webabruf_YAHOO($curxchg, $self->{Kurs}{$curxchg});
                      # Normalisieren des Wertes
                      $self->{Kurs}{$curxchg}{last} *= 1;
                      # Fuer GBP ist ein weiterer Faktor 100 noetig
                      $self->{Kurs}{$curxchg}{last} *= 100 if ($$hashptr{_Waehrung} eq 'GBP');
                    }
                  } ## end if ($$hashptr{_Waehrung...})
                } ## end if (!$self->{Kurs}{$curxchg...})

                # Der Wert in EUR wird nur ermittelt, falls ein Wechselkurs existiert
                $wert = $wert / $self->{Kurs}{$curxchg}{last} if ($self->{Kurs}{$curxchg}{last});
              } else {
                $wert = $wert * $self->{Flags}{$flag}{Faktor};
              }
            } ## end if ($self->{Flags}{$flag...})
            # Sichern des faktorisierten Wertes
            $$hashptr{$flagname} = ($flagname ne 'symbol') ? $wert : (split(/\./, $wert))[0];
          } ## end if (defined($flag))
        } ## end while ($stockdata =~ /\"[\s0]*([^\"]+?)[\s]*\"|[\s0]*([^,\s]+)/g)
      } ## end if (defined($stockdata...))
      $maxtry--;
    }
    until ($maxtry <= 0 || ($$hashptr{last} &&
                            $$hashptr{last} ne 'N/A' &&
                            $$hashptr{last} > 0 &&
                            $$hashptr{last} <= 1000000 &&
                            $$hashptr{last} != 1));

    if ($$hashptr{date} && $$hashptr{'time'}) {
      my $tradetime = str2time("$$hashptr{date} $$hashptr{'time'}", $self->{TZ}) || 0;
      $$hashptr{_Tradetime} = time2str('%d.%m.%y %R', $tradetime);
      $$hashptr{_lastTrade} = abs((str2time(time2str('%x', time)) - str2time(time2str('%x', $tradetime))) / 86400);

      if ($tradetime < str2time(time2str('%x', time))) {

        # $$hashptr{_Tradetime} .= '<';
        $$hashptr{_aktuell} = ($$hashptr{_letzter_Abruf} && $$hashptr{_letzter_Abruf} < time - $self->{MaxDelay}) ? 0 : 1;
      } else {$$hashptr{_aktuell} = 1}
      $$hashptr{_letzter_Abruf} = time;
    } else {$$hashptr{_Tradetime} = 0}

    if ($$hashptr{name}) {$$hashptr{name} =~ s/\s+N$//}

    Trace->Trc('I', 2, 0x02102, $symbol, $$hashptr{name}, $$hashptr{last});

    $/ = $crlfmerker;
  } ## end if (!$$hashptr{_aktuell...})

  # Kursinfos speichern
  if (exists($self->{VarFile})) {store $self->{Kurs}, $self->{VarFile}}

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub _webabruf_YAHOO


sub _webabruf_QUOTE {
  #################################################################
  # Durchfuehren eines Webabrufs

  #
  # Prozedurnummer 1
  #
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 3, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;
  
  # Array ueber alle Symbole erstellen
  my @markets = split(' ', Configuration->config('Stockservice', 'Markets'));
  if (!@markets) {@markets = (qw/vwd europe/)};

  my %symbolhash;
  # Waehrungen aus %kurs extrahieren
  foreach (keys %{$self->{Kurs}}) {
    foreach my $symbol (split('\|', $_)) {$symbolhash{$symbol} = $_}
  }

  my $quoter = Finance::Quote->new();
  $quoter->set_currency(EUR);  # Set default currency.
    
  foreach my $quelle (@markets) {
    my @symbols;
    foreach (sort keys %symbolhash) {push(@symbols, $_) if (!$self->{Kurs}{$symbolhash{$_}}{_aktuell})}
    if (scalar(@symbols)) {
      Trace->Trc('I', 2, 0x02101, $quelle, join(' ', @symbols));
      my %info = $quoter->fetch($quelle, @symbols);

      # Ergebnisse auswerten und @symbol aktualisieren
      foreach my $symbol (@symbols) {
        next unless $info{$symbol,"success"}; # Skip failures.
        next unless $info{$symbol,"last"} ne '0,00'; # Skip failures.
        my $hashptr = $self->{Kurs}{$symbolhash{$symbol}};
        my %value;
        foreach (keys %{$self->{Flags}}) {
          Trace->Trc('I', 2, 0x02102, $symbol, $_, $info{$symbol, $_});
          if (defined($info{$symbol, $_})) {
            if (defined($self->{Flags}{$_}{Laenge})) {$value{$_} = substr($info{$symbol, $_}, 0, $self->{Flags}{$_}{Laenge})}}  
          else {
            if (defined($self->{Flags}{$_}{Default})) {$value{$_} = $self->{Flags}{$_}{Default}}
          }
        }
      
        my $tradetime = 0;
        if ($value{date} && $value{'time'}) {$tradetime = str2time("$value{date} $value{'time'}") || 0}
        
        if (!defined($$hashptr{_tradetime}) || ($tradetime > $$hashptr{_tradetime})) {
          $$hashptr{_aktuell} = 0;
      
          foreach my $flagname (keys(%value)) {
            if ($self->{Flags}{$flagname}{Faktor}) {$$hashptr{$flagname . '_RAW'} = $value{$flagname}}
            $$hashptr{$flagname} = ($flagname ne 'symbol') ? $value{$flagname} : (split(/\./, $value{$flagname}))[0];
          } ## end if (defined($flag))
      
          if ($tradetime) {
            $$hashptr{_tradetime} = $tradetime;
            $$hashptr{_Tradetime} = time2str('%d.%m.%y %R', $$hashptr{_tradetime} + $self->{UTCDelta});
            $$hashptr{_lastTrade} = abs((str2time(time2str('%x', time)) - str2time(time2str('%x', $tradetime))) / 86400);
            $$hashptr{_aktuell} = 1;
      
      #      if (($tradetime > (str2time(gmtime()) - $self->{MaxDelay})) ||
      #          (defined($$hashptr{_letzter_Abruf}) && ($$hashptr{_letzter_Abruf} > (str2time(gmtime()) - $self->{MaxDelay})))) {
      #        $$hashptr{_aktuell} = 1;
      #      }
      
            $$hashptr{_letzter_Abruf} = str2time(gmtime());
          } else {$$hashptr{_Tradetime} = 0}
      
          if ($$hashptr{name}) {$$hashptr{name} =~ s/\s+N$//}
          if (!defined($$hashptr{symbol}) || ($$hashptr{symbol} eq '')) {$$hashptr{symbol} = $symbol}
          if (!defined($$hashptr{exchange}) || ($$hashptr{exchange} eq '')) {$$hashptr{exchange} = $quelle}
        }
        Trace->Trc('I', 2, 0x02102, $symbol, $$hashptr{name}, $$hashptr{last});
      }
    }
  }

  # Kursinfos speichern
  if (exists($self->{VarFile})) {store $self->{Kurs}, $self->{VarFile}}

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return 0;
} ## end if (!$$hashptr{_aktuell...})


sub lese_Portofolios {
  #####################################################################
  # Einlesen der Portofolios mit Inhalten
  # Ergebnis: Datenstruktur $self->{Portofolios} mit folgendem Aufbau
  # $self->{Portofolios}->{<Portotfolioname>}->{<Positionsname>}
  #     Branche         : Branche
  #     Quantity        : Amount of Shares
  #     Dividende_Share : Dividende/Share
  #     Price_Buy       : Buying Price of whole Position
  #     Price_Buy_Share : Buying Price/Share
  # und
  # $self->{Portofolios}->{<Portotfolioname>}->{<Positionsname>}
  #     Branche         : Branche
  #     Quantity        : Amount of Shares
  #     Dividende_Share : Dividende/Share
  #     Price_Buy       : Buying Price of whole Position
  #     Price_Buy_Share : Buying Price/Share
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
      Trace->Trc('I', 2, 0x02201, $_);
      my %PFList;
      if (-r $PFFiles{$_} && tie(%PFList, 'Config::IniFiles', (-file => $PFFiles{$_}))) {
        foreach my $current (keys %PFList) {
          # Sammle alle Portofolios oder Watchlists inklusive Namen aus den Konfigurationsfiles
          if ($current =~ /^(Portofolio|Watchlist) .*$/) {
            Trace->Trc('I', 2, 0x02202, $_, $current);
            if (defined($PFHash{$current})) {
              # Portofolio oder Teile davon wurden schon mal vorher definiert
              foreach my $pos (keys %{$PFList{$current}}) {
                Trace->Trc('I', 2, 0x02203, $_, $current, $pos);
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
          } ## end if ($current...)
        } ## end foreach my $current...
      } ## end if (-r $PFFiles...)
    } ## end foreach (keys(%PFFiles...))
  } ## end f (Configuration->conf...

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
} ## end sub lese_Portofolios


sub extract_Cash {
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
      if ($pos !~ /^CASH/) {
        Trace->Trc('I', 2, "Bearbeite aus Portofolio <%s> Share-Position <%s>.", $PFName, $pos);
        $PFHash{$PFName}{$pos} = $tmpHash{$PFName}{$pos};
      } else {
        if ($PFName =~ /^Portofolio .*$/ && $pos =~ /^CASH\.(.*)\.(.*)$/) {
          my ($owner, $bank) = ($1, $2);
          if (defined($owner) && defined($bank)) {
            my ($cur, $value) = split(/\|/, $tmpHash{$PFName}{$pos});
            if (!defined($value)) {
              $value = $cur;
              $cur   = 'EUR';
            }
            Trace->Trc('I', 2, 0x02212, $PFName, $pos, $owner, $bank);
            $cash{$owner}{$bank}{$cur} += $value;
            $cash{$owner}{Summe}{$cur} += $value;
            $cash{Bank}{$bank}{$cur}   += $value;
            $cash{Bank}{Summe}{$cur}   += $value;
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
} ## end sub extract_Cash


sub erzeuge_Gesamtliste {
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
  foreach my $PFName (%PFHash) {
    if ($PFName =~ /^Portofolio .*$/) {
      foreach my $pos (keys %{$PFHash{$PFName}}) {
        Trace->Trc('I', 2, 0x02203, $PFName, $pos);
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
  $PFHash{"Watchlist $self->{Gesamtliste}"} = \%gesamtliste;

  $self->{PFHash} = \%PFHash;
  
  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub erzeuge_Gesamtliste


sub parse_Positionen {
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
  
  if (exists($self->{VarFile})) {
    eval {$self->{Kurs} = retrieve($self->{VarFile});};
  }

  # Datenstruktur zum Sammeln der Kursinformatinen zu allen Positionen anlegen
  # und mit Werten aus der INI fuellen
  foreach my $PFName (sort(keys(%PFHash))) {
    # Fuer jedes gefundene Protofolio/jede Watchlist, der/die Elemente (Aktienpositionen) enthaelt
    if (scalar keys %{$PFHash{$PFName}}) {
      foreach my $pos (keys %{$PFHash{$PFName}}) {
        Trace->Trc('I', 4, 0x02204, $PFName, $pos, $PFHash{$PFName}{$pos});
        my @stockattributearray;
        if (ref($PFHash{$PFName}{$pos})) {
          # Falls die Position mehrfach vorkommt sammel alle Vorkommen in einem Array
          @stockattributearray = @{$PFHash{$PFName}{$pos}};
        } else {
          # Falls die Position nur einmal vorkommt schieb das Element in den Array
          push(@stockattributearray, $PFHash{$PFName}{$pos});
        }
        if ($self->{SumPos} && $#stockattributearray) {
          # Falls gewuenscht, Aufsummieren aller Einzelpositionen so, dass am Ende nur noch eine Arrayelement mit allen aufsummierten Positionen uebrig bleibt
          my ($Currency, $Price_Buy, $Quantity, $Dividende, $Branche, $Kurs) = ('', 0, 0, 0, '', 0);
          while ($#stockattributearray >= 0) {
            my @stockattribute = split('\|', shift(@stockattributearray));
            Trace->Trc('I', 3, 0x02205, $PFName, $pos, $stockattribute[0] || '', $stockattribute[1] || '', $stockattribute[2] || '', $stockattribute[3] || '', $stockattribute[4] || '');
            $Currency   = $stockattribute[0] || $Currency  || 'EUR';
            $Quantity  += $stockattribute[1] || 0;
            $Price_Buy += $stockattribute[2] || 0;
            $Dividende  = $stockattribute[3] || $Dividende || 0;
            $Branche    = $stockattribute[4] || $Branche   || '';
            $Kurs       = $stockattribute[5] || $Kurs      || 0;
          }
          $stockattributearray[0] = join('|', ($Currency, $Quantity, $Price_Buy, $Dividende, $Branche, $Kurs));
          Trace->Trc('I', 2, 0x02206, $PFName, $pos, $Quantity);
        } ## end if ($self->{SumPos} &&...)
        my $poscount = 0;
        foreach my $exg_anz_prz (@stockattributearray) {
          my @stockattribute = split('\|', $exg_anz_prz);
          $poscount++;
          my $ppos = "$pos $poscount";
          $self->{Kurs}{$pos}{_Waehrung}                        = $stockattribute[0] || 'EUR';
          $self->{Kurs}{$pos}{_last}                            = 0;
          $self->{Portofolios}{$PFName}{$ppos}{Quantity}        = $stockattribute[1];
          $self->{Portofolios}{$PFName}{$ppos}{Price_Buy}       = $stockattribute[2];
          $self->{Portofolios}{$PFName}{$ppos}{Dividende_Share} = $stockattribute[3] || 0;
          $self->{Portofolios}{$PFName}{$ppos}{Branche}         = $stockattribute[4] || '';
          $self->{Kurs}{$pos}{_last}                            = $stockattribute[5] if (defined($stockattribute[5]) && $stockattribute[5] =~ m/^([-+]?)([0-9]*).*$/);
          
          if (looks_like_number($self->{Portofolios}{$PFName}{$ppos}{Quantity}) && $self->{Portofolios}{$PFName}{$ppos}{Quantity} && 
              looks_like_number($self->{Portofolios}{$PFName}{$ppos}{Price_Buy})) {
            $self->{Portofolios}{$PFName}{$ppos}{Price_Buy_Share} = $self->{Portofolios}{$PFName}{$ppos}{Price_Buy} / $self->{Portofolios}{$PFName}{$ppos}{Quantity};
          } else {
            Trace->Trc('I', 2, 0x0220f, $self->{Portofolios}{$PFName}{$ppos}{Quantity}, looks_like_number($self->{Portofolios}{$PFName}{$ppos}{Quantity}), $self->{Portofolios}{$PFName}{$ppos}{Price_Buy}, looks_like_number($self->{Portofolios}{$PFName}{$ppos}{Price_Buy}));
            $self->{Portofolios}{$PFName}{$ppos}{Price_Buy_Share} = '';
          }
        } ## end foreach my $exg_anz_prz (@stockattributearray)
      } ## end foreach my $pos (keys %{$PFHash...})
    } ## end if (scalar keys %{$PFHash...})
  } ## end foreach my $PFName ...
  
  delete($self->{PFHash});

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub parse_Positionen


sub ergaenze_Wechselkurse {
  #################################################################
  # Ergaenzt den Kurshash um die Wechselkurseintraege
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  my %helper;
  foreach my $symbol (keys %{$self->{Cash}->{Bank}->{Summe}}) {
    $helper{'EUR' . $symbol . '=X'} = $symbol;
  }
  foreach (keys %{$self->{Kurs}}) {
    foreach my $symbol (split('\|', $_)) {
      $helper{'EUR' . $self->{Kurs}{$_}{_Waehrung} . '=X'} = $self->{Kurs}{$_}{_Waehrung};
    }
  }
  
  foreach (keys(%helper)) {
    $self->{Kurs}{$_}{_Waehrung} = $helper{$_};
    $self->{Kurs}{$_}{_last}     = 0;
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Kurse_ermitteln


sub Kurse_ermitteln {
  #################################################################
  # Ermittelt Kurse zu allen Papieren, die in den Depots gefunden 
  # wurden
  # Ergebnis: In der Datenstruktur $self->{Kurse} liegt
  # ein Hash (Portofolios) von Hashes (Position) mit:
  #
  # Quantity          : Amount of Shares in Position

  # Price_Buy         : Buying Price of whole Position
  # Price_Buy_Share       : Buying Price for one Share

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

  # Methode YAHOO
  if (Configuration->config('Stockservice', 'Source') eq 'YAHOO') {
    foreach (sort keys %{$self->{Kurs}}) {
      my @symbols = split('\|', $_);
      while ((my $symbol = shift(@symbols)) && !($self->{Kurs}{$_}{last})) {
        # Fuer jedes Symbol den Abruf machen sofern noch kein Kurs feststeht
        $self->_webabruf_YAHOO($symbol, $self->{Kurs}{$_}, \(keys %{$self->{Flags}}));
        if ($self->{Kurs}{$_}{_last}) {
          $self->{Kurs}{$_}{last} = $self->{Kurs}{$_}{_last}
        }
      }
    }
  }

  # Methode Finance::Quote
  if (Configuration->config('Stockservice', 'Source') eq 'Finance::Quote') {
    # Array ueber alle Symbole erstellen
    my @markets = split(' ', Configuration->config('Stockservice', 'Markets'));
    if (!@markets) {@markets = (qw/vwd europe/)};

    my %symbolhash;
    # Waehrungen aus %kurs extrahieren
    foreach (keys %{$self->{Kurs}}) {
      foreach my $symbol (split('\|', $_)) {
        $symbolhash{$symbol} = $_;
      }
    }

    $self->_webabruf_QUOTE();
    
    foreach my $symbol (keys %{$self->{Kurs}}) {
      if ($self->{Kurs}{$symbol}{_Waehrung} ne 'EUR') {
        foreach my $EUR (keys %{$self->{Kurs}{$symbol}}) {
          if (defined($self->{Kurs}{$symbol}{$EUR . '_RAW'})) {
            my $xsymbol = 'EUR' . $self->{Kurs}{$symbol}{_Waehrung} . '=X';
            if ($self->{Kurs}{$xsymbol}{last}) {
              Trace->Trc('I', 5, 0x02103, $xsymbol, $self->{Kurs}{$xsymbol}{last});
              $self->{Kurs}{$symbol}{$EUR . '_RAW'} = $self->{Kurs}{$symbol}{$EUR} * $self->{Kurs}{$xsymbol}{last}
            }
          }
        }
      }
    }
  }
  
  # Kurse sollten abgeholt und in $self->{Kurs} gespeichert sein
  # jetzt bereinigen wir die geladenen Werte und setzen ggf. Defaults
  foreach my $symbol (keys(%{$self->{Kurs}})) {
    $self->{Kurs}{$symbol}{net} = 0 if (!defined($self->{Kurs}{$symbol}{net}));
    $self->{Kurs}{$symbol}{div} = 0 if (!defined($self->{Kurs}{$symbol}{div}));
    foreach my $attribute (keys(%{$self->{Kurs}{$symbol}})) {
      $self->{Kurs}{$symbol}{$attribute} =~ s/^\s*|\s*$//g
    }
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Kurse_ermitteln


sub _calculate {
  my $self = shift;
  
  my $depot = shift;

  # Falls es wir ein echtes Depot bearbeiten
  foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
    if (my $symbol = (split(' ', $pos))[0]) {
      # Falls es eine Position gibt
      $self->{Portofolios}{$depot}{$pos}{Price_Share}        = $self->{Kurs}{$symbol}{last} ? $self->{Kurs}{$symbol}{last} : $self->{Portofolios}{$depot}{$pos}{Price_Buy_Share};
      $self->{Portofolios}{$depot}{$pos}{Price}              = $self->{Portofolios}{$depot}{$pos}{Quantity} * $self->{Portofolios}{$depot}{$pos}{Price_Share};
      $self->{Portofolios}{$depot}{$pos}{Change}             = $self->{Portofolios}{$depot}{$pos}{Price} - $self->{Portofolios}{$depot}{$pos}{Price_Buy};
      $self->{Portofolios}{$depot}{$pos}{Change_Percent}     = $self->{Portofolios}{$depot}{$pos}{Price_Buy} ? 100 * $self->{Portofolios}{$depot}{$pos}{Change} / $self->{Portofolios}{$depot}{$pos}{Price_Buy} : 0;
      $self->{Portofolios}{$depot}{$pos}{Change_Day}         = $self->{Portofolios}{$depot}{$pos}{Quantity} * $self->{Kurs}{$symbol}{net};
      $self->{Portofolios}{$depot}{$pos}{Change_Day_Percent} = $self->{Kurs}{$symbol}{p_change};
      $self->{Portofolios}{$depot}{$pos}{Price_Last}         = $self->{Portofolios}{$depot}{$pos}{Price} - $self->{Portofolios}{$depot}{$pos}{Change_Day};
  
      $self->{Portofolios}{$depot}{$pos}{Dividende}          = $self->{Portofolios}{$depot}{$pos}{Quantity} * $self->{Portofolios}{$depot}{$pos}{Dividende_Share};
      $self->{Portofolios}{$depot}{$pos}{Dividende_Percent}  = $self->{Portofolios}{$depot}{$pos}{Price} ? 100 * $self->{Portofolios}{$depot}{$pos}{Dividende} / $self->{Portofolios}{$depot}{$pos}{Price} : 0;
      $self->{Portofolios}{$depot}{$pos}{Div_Summe}          = $self->{Portofolios}{$depot}{$pos}{Quantity} * $self->{Kurs}{$symbol}{div};

      # Price_Buy, Price, Change_Day und Dividende werden fuer die Summe aufaddiert, fuer den Rest ermittelt
      $self->{Portofolios}{Summe}{$depot}{Price_Buy}        += $self->{Portofolios}{$depot}{$pos}{Price_Buy};
      $self->{Portofolios}{Summe}{$depot}{Price}            += $self->{Portofolios}{$depot}{$pos}{Price};
      $self->{Portofolios}{Summe}{$depot}{Price_Last}       += $self->{Portofolios}{$depot}{$pos}{Price_Last};
      $self->{Portofolios}{Summe}{$depot}{Change_Day}       += $self->{Portofolios}{$depot}{$pos}{Change_Day};
      $self->{Portofolios}{Summe}{$depot}{Dividende}        += $self->{Portofolios}{$depot}{$pos}{Dividende} ;
    }
  }
  foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
    $self->{Portofolios}{$depot}{$pos}{Portion}            =  $self->{Portofolios}{Summe}{$depot}{Price} ? 100 * $self->{Portofolios}{$depot}{$pos}{Price} / $self->{Portofolios}{Summe}{$depot}{Price} : 100;
  }

  # Im Anschluß die Depotsummenwerte bearbeiten
  $self->{Portofolios}{Summe}{$depot}{Change}               = $self->{Portofolios}{Summe}{$depot}{Price} - $self->{Portofolios}{Summe}{$depot}{Price_Buy};
  $self->{Portofolios}{Summe}{$depot}{Change_Percent}       = $self->{Portofolios}{Summe}{$depot}{Price_Buy}  ? 100 * $self->{Portofolios}{Summe}{$depot}{Change} / $self->{Portofolios}{Summe}{$depot}{Price_Buy}       : 0;
  $self->{Portofolios}{Summe}{$depot}{Change_Day_Percent}   = $self->{Portofolios}{Summe}{$depot}{Price_Last} ? 100 * ($self->{Portofolios}{Summe}{$depot}{Price} / $self->{Portofolios}{Summe}{$depot}{Price_Last} - 1) : 100;
  $self->{Portofolios}{Summe}{$depot}{Dividende_Percent}    = $self->{Portofolios}{Summe}{$depot}{Price_Buy}  ? 100 * $self->{Portofolios}{Summe}{$depot}{Dividende} / $self->{Portofolios}{Summe}{$depot}{Price}        : 0;
}


sub analysiere_Portofolios {
  #################################################################
  # Ermittelt Kennzahlen zu den Portofolios
  # Ergebnis: In der Datenstruktur $self->{Portofolios} liegt
  # ein Hash (Portofolios/Watchlists/Summe) von Hashes (Aktienposition) mit:
  #
  # PW  Rating             : Hash aus Portion, Yield, Win, DayWin, LastTrade, Neutral
  # PW  Branche            : Branche
  # PW  Quantity           : Amount of Shares in Position
  # PWS Price              : Aktueller Preis der Position
  # PWS Price_Buy          : Kaufpreis der Position
  # PWS Price_Buy_Share    : Kaufpreis einer Aktie
  # PWS Change             : Absolute Veraenderung
  # PWS Change_Percent     : Prozentuale Veraenderung
  # PWS Change_Day         : Absolute Veraenderung am aktuellen Tag
  # PWS Change_Day_Percent : Prozentuale Veraenderung am aktuellen Tag
  # PWS Div_Summe          : Dividenden Summe ermittelt aus der Stockinfo
  # PWS Dividende          : Absolute Dividende
  # PWS Dividende_Share    : Dividende per Share
  # PWS Dividende_Percent  : Prozentuale Dividende bezogen auf den aktuellen Wert

  # PWS Price_EUR          : Current Price of whole Position in EUR

  # Depot_Weight           : Percentage Depot Weight of Position
  # _PercentDepotDayChange : Percentage daily Depot Price Change of Position in Total
  # _PercentDepotChange    : Percentage Depot Price Change of Position in Total

  # _PercentDayChange      : Daily percentage Change
  # _PercentChange         : Total percentage Change
  #

  #
  # Prozedurnummer 4
  #
  my $self = shift;

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  no autovivification;
  
#  sub spf ($$) {
#    my $format = $_[0];
#    my $text   = $_[1] || 0;
#
#    my $len = 5;
#    if ($format =~ m/^%([-+]?)([0-9]*).*$/) {
#      $len = $2;
#      $len++ if $1 eq '+';
#      $len = '%' . $len . 's';
#    }
#    $text = sprintf($format, $text);
#    $text = reverse $text;
#    $text =~ s:\.:,:g; # deutsches Komma als Dezimaltrenner
#    $text =~ s/(\d\d\d)(?=\d)(?!\d*,)/$1./g;
#    $text = scalar reverse $text;
#    $text =~ s/^\s+|\s+$//g;
#    $text = sprintf($len, $text);
#    return $text;
#  }

  sub rating {
    my $mode   = shift || 'asc';
    my $value  = shift || 0;
    my $red    = shift || 0;
    my $yellow = shift || 0;
    my $blue   = shift || 0;
    my $green  = shift || 0;
    
    if ($mode eq "asc") {
      return 'mm'      if $value <= $red;
      return 'm'       if $value <= $yellow;
      return 'neutral' if $value < $blue;
      return 'p'       if $value < $green;
      return 'pp';
    }
    
    if ($mode eq "desc") {
      return 'mm'      if $value <= $green;
      return 'm'       if $value <= $blue;
      return 'neutral' if $value < $yellow;
      return 'p'       if $value < $red;
      return 'pp';
    }
    
    if ($mode eq 'range') {
      return 'mm'      if $value <= $red;
      return 'mm'      if $value >= $green;
      return 'neutral' if $value <= $yellow;
      return 'neutral' if $value >= $blue;
      return 'pp';
    }
  }

  my $rc = 0;

  POSIX::setlocale(&POSIX::LC_ALL, 'de_DE');
  $self->{Portofolios}{Summe} = {};
  # Berechnung und Aufsummieren der Positionswerte
  foreach (keys %{$self->{Portofolios}}) {
    next if ($_ eq 'Summe');
    $self->_calculate($_);
  }
  
  # Gewichtung der Portofolios berechnen
  foreach (keys %{$self->{Portofolios}{Summe}}) {
    if ($_ =~ /Portofolio .*/) {
      $self->{Portofolios}{Summe}{$_}{Portion} = $self->{Portofolios}{Summe}{"Watchlist $self->{Gesamtliste}"}{Price} ? 100 * $self->{Portofolios}{Summe}{$_}{Price} / $self->{Portofolios}{Summe}{"Watchlist $self->{Gesamtliste}"}{Price} : 0;
    }
  }
  foreach my $depot (keys %{$self->{Portofolios}}) {
    # Die Werte aller Positionen sind ermittelt.
    # Jetzt kann die Rating und Formatierung aller Positionen erfolgen
    foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
      my $symbol = (split(' ', $pos))[0];
      # Position bewerten
      $self->{Portofolios}{$depot}{$pos}{Rating}{LastTrade} = $symbol ? rating('desc',  defined($self->{Kurs}{$symbol}{_lastTrade}) ? $self->{Kurs}{$symbol}{_lastTrade} : 0, 3, 2, 1, 0) : 0;
      $self->{Portofolios}{$depot}{$pos}{Rating}{Win}       = rating('asc',   $self->{Portofolios}{$depot}{$pos}{Change_Percent}, -10, -5, 5, 10);
      $self->{Portofolios}{$depot}{$pos}{Rating}{DayWin}    = rating('asc',   $self->{Portofolios}{$depot}{$pos}{Change_Day_Percent}, -2, -1, 1, 2);
      $self->{Portofolios}{$depot}{$pos}{Rating}{Portion}   = rating('range', $self->{Portofolios}{$depot}{$pos}{Portion}, 0, 1, 3, 6, 10);
      $self->{Portofolios}{$depot}{$pos}{Rating}{Yield}     = rating('asc',   $self->{Portofolios}{$depot}{$pos}{Dividende_Percent}, 1, 2, 3, 4);
    }
  }

  foreach my $symbol (keys %{$self->{Kurs}}) {
    foreach my $flagname (keys %{$self->{Kurs}{$symbol}}) {
      if ($self->{Flags}{$flagname}{Format}) {
        # @@@
        $self->{Kurs}{$symbol}{$flagname} = 0 if ($flagname =~ /^p_/ && $self->{Kurs}{$symbol}{$flagname} =~ /%$/);
        Trace->Trc('I', 5, 0x02210, $symbol, $flagname, $self->{Kurs}{$symbol}{$flagname}, $self->{Flags}{$flagname}{Format});
#        $self->{Kurs}{$symbol}{$flagname} = sprintf($self->{Flags}{$flagname}{Format}, $self->{Kurs}{$symbol}{$flagname});
      }
    }
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub analysiere_Portofolios


sub _createFormat {
  #################################################################
  # Schreiben der Header Ausgabedatei(en)
  # Ergebnis:
  #

  # Prozedurnummer 5
  #

  my $self = shift;
  
  my %args = ('Ausgabe'  => '', 
              'Part'     => '', 
              'Depot'    => '', 
              'Position' => '',
              @_);
                
  my $ausgabe = $args{'Ausgabe'};
  my $part    = $args{'Part'};
  my $depot   = $args{'Depot'};
  my $pos     = $args{'Position'};
  
  $pos = 'Watchlist Summe' if ($pos eq 'Summe');
  my $symbol = (split(' ', $pos))[0];

  my %formatHash;
  if ($part eq 'Head') {
    $formatHash{Portofolio} = $pos if defined($pos);
  }
  if ($part eq 'Data') {
    $formatHash{Portofolio} = $depot if defined($depot);
  }
  $formatHash{Name}       = $pos if defined($pos);

  # Sharedefinitionen
  if (defined($self->{Kurs}{$symbol})) {
    foreach (keys %{$self->{Kurs}{$symbol}}) {
      $formatHash{$_} = $self->{Kurs}{$symbol}{$_} if (defined($self->{Kurs}{$symbol}{$_}));
    }
  }

  # Depotdefinitionen
  foreach (keys %{$self->{Portofolios}{$depot}{$pos}}) {
    if (!ref($self->{Portofolios}{$depot}{$pos}{$_}) && defined($self->{Portofolios}{$depot}{$pos}{$_})) {
      $formatHash{$_} = $self->{Portofolios}{$depot}{$pos}{$_};
    }
  }

  # Konsolensteuerung (Farbe):
  # W : Aenderung seit Kauf
  # D : Tagesaenderung
  # P : Portion am Gesamtvermoegen
  # Y : Dividende
  # L : Letzter Kurs veraltet
  foreach (keys %{$self->{Portofolios}{$depot}{$pos}{Rating}}) {
    my $col = color($self->{FG}->{$self->{Portofolios}{$depot}{$pos}{Rating}{$_}});
    $formatHash{substr($_, 0, 1)} = $col if (defined($col));
  }
  # C : Ruecksetzen der Farbe
  $formatHash{C} = color($self->{FG}->{'neutral'});
  
  # Ersetzen der '|' durch '/'
  foreach (keys(%formatHash)) {
    $formatHash{$_} =~s/\|/\//g;
  }

  return join('|', %formatHash);
}


sub _push {
  #################################################################
  # Schreiben der Header Ausgabedatei(en)
  # Ergebnis:
  #

  # Prozedurnummer 5
  #

  my $self = shift;
 
  my $ausgabe = shift;
  my $depot   = shift;
  my $part    = shift;

  return if !defined(Configuration->config("Ausgabeformat_$ausgabe", $part));
  
  if ($part eq 'Data') {
    # Ausgabe sortiert nach Name
    # Falls keine Kurs existert erfolgt auch keine Ausgabe
    # Ausser bei der Summenausgabe
    my %sortlist;
    foreach (keys %{$self->{Portofolios}{$depot}}) {
      if ($depot eq 'Summe') {
        next if ($_ eq 'Watchlist Summe');  # Das Watchlist Depot mit der Summe aller Depotpositionen soll nicht in der Auflistung der Depots erscheinen
        $sortlist{uc($depot . $_)} = $_;
      } else {
        my $symbol = (split(' ', $_))[0];
        if ($self->{Kurs}{$symbol}{name}) {
          $sortlist{uc($self->{Kurs}{$symbol}{name} . $_)} = $_;
        }
      }
    }

    foreach (sort keys %sortlist) {
      my $format = $self->_createFormat('Ausgabe',  $ausgabe,
                                        'Part',     'Data',
                                        'Depot',    $depot,
                                        'Position', $sortlist{$_});  
      Trace->Trc('I', 10, "Format: <$format> wird angewendet auf Ausgabe: <" . Configuration->config("Ausgabeformat_$ausgabe", 'Data') . ">.");
      my $ausgabestring = Utils::extendString(Configuration->config("Ausgabeformat_$ausgabe", 'Data'), $format);
      Trace->Trc('I', 10, "Schreibe Data: <$ausgabe>  Depot: <$depot>: <$ausgabestring>");
      push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, $ausgabestring);
    }
  } elsif ($part eq 'Head') {
    my $format = $self->_createFormat('Ausgabe',  $ausgabe,
                                      'Part',     'Head',
                                      'Depot',    'Summe',
                                      'Position', $depot);  
    my $ausgabestring = Utils::extendString(Configuration->config("Ausgabeformat_$ausgabe", 'Head'), $format);
    Trace->Trc('I', 5, "Schreibe Head: <$ausgabe>  Depot: <$depot>: <$ausgabestring>");
    push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, $ausgabestring);
  } else {
    push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, Utils::extendString(Configuration->config("Ausgabeformat_$ausgabe", $part), "C|" . color($self->{FG}->{'neutral'})));
  }
  
  return;
}


sub schreibe_Ausgabe {
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

  my %ausgabedatei = Configuration->config('Ausgabedatei');

  # Abarbeitung aller Portofolios
  foreach my $depot (keys %{$self->{Portofolios}}) {
    foreach $ausgabe (keys %ausgabedatei) {
      next if (($depot eq 'Summe') && ($ausgabe ne 'Summe')); # Depot Summe wird nur auf Ausgabe Summe geschrieben
      next if (($depot ne 'Summe') && ($ausgabe eq 'Summe')); # und sonst nichts
      Trace->Trc('I', 2, "Erzeuge Ausgabe fuer <$ausgabe>  Depot: <$depot>  Datei: <$ausgabedatei{$ausgabe}>.");
      $self->_push($ausgabe, $depot, 'Head');
      $self->_push($ausgabe, $depot, 'Sep');
      $self->_push($ausgabe, $depot, 'Col');
      $self->_push($ausgabe, $depot, 'Sep');
      $self->_push($ausgabe, $depot, 'Data');
    }
  }

  # Ausgabedateien schreiben
  foreach my $ausgabe (keys %{$self->{Ausgabe}}) {
    foreach my $depot (keys %{$self->{Ausgabe}{$ausgabe}}) {
      Trace->Trc('I', 2, 0x02501, $depot, Utils::extendString($ausgabedatei{$ausgabe}, "Depot|$depot"));
      if (!CmdLine->argument(0) || $depot ne 'Summe') {

        # Anlegen
        #Trace->Log($ausgabe, Utils::extendString($ausgabedatei{$ausgabe} . '.tmp', "Depot|$depot")) or Trace->Exit(1, 0, 0x00010, $ausgabedatei{$ausgabe}, "Depot|$depot");
        my $dateiname = Utils::extendString($ausgabedatei{$ausgabe}, "Depot|$depot");
        eval {mkpath(dirname($dateiname));};
        open(DEPOT, '> ' . $dateiname . '.tmp');
        while (my $line = shift(@{$self->{Ausgabe}{$ausgabe}{$depot}})) {
          print DEPOT "$line\n";
        }

        # Schliessen und Umbenennen
        #$self->TerminateLog($ausgabe);
        close(DEPOT);
        move($dateiname . '.tmp', $dateiname);
      } ## end if (!CmdLine->argument...)
    } ## end foreach my $depot (keys %{$self...})
  } ## end foreach my $ausgabe (keys %...)

  # Kursinfos löschen
  if (exists($self->{VarFile})) {
    eval {unlink($self->{VarFile});};
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub schreibe_Ausgabe

#sub _schreibe_Ausgabe {
#  #################################################################
#  # Schreiben der Ausgabedatei(en)
#  # Ergebnis:
#  #
#
#  # Prozedurnummer 5
#  #
#
#  my $self = shift;
#
#  my $merker = $self->{subroutine};
#  $self->{subroutine} = (caller(0))[3];
#  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});
#
#  no autovivification;
#
#  my $rc = 0;
#  my ($dateiname, $format, $symbol);
#  
#  my $bg = $self->{BG}->{normal};
#  # Farbdefinition Background:
#  # N : Ruecksetzen der Farbe
#  # W : Aenderung seit Kauf
#  # D : Tagesaenderung
#  # P : Portion am Gesamtvermoegen
#  # Y : Dividende
#  # L : Letzter Kurs veraltet
#  my %fg = %{$self->{FG}};
#
#  # Abarbeitung aller Portofolios
#  foreach my $depot (keys %{$self->{Portofolios}}) {
#    # Ausgabe der Summeninformation und der Kopfzeile
#    # Festsetzung Hintergrund und Aufbau Formatstring
#    $bg     = $self->{BG}->{normal};
#    $format = "Portofolio|$depot|";
#    foreach my $pos (keys %{$self->{Portofolios}{Summe}{$depot}}) {
#      $format .= "$pos|$self->{Portofolios}{Summe}{$depot}{$pos}|" if (!ref($self->{Portofolios}{Summe}{$depot}{$pos}));
#    }
#    foreach my $rating (keys %{$self->{Portofolios}{Summe}{$depot}{Rating}}) {
#      $format .= "$rating|".color($fg{$self->{Portofolios}{Summe}{$depot}{Rating}{$rating}})."|";
#    }
#
#    # Ausgabe der Header fuer TXT und CSV puffern
#    foreach my $ausgabe (keys %ausgabedatei) {
#      if ($self->{Portofolios}{Summe}{$depot}) {
#        # Unterscheidung Depotuebersicht oder Einzeldepot
#        my $summe = ($depot eq 'Summe') ? '_S' : '_';
#        if (Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Head')) {
#          my $line = (Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Trenn')) ? Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Trenn') : '';
#          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, Utils::extendString(Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Head'), $format));
#          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, $line);
#          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, Utils::extendString(Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Col'),  $format));
#          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, $line);
#        }
#      } ## end if ($self->{Portofolios...})
#    } ## end foreach my $ausgabe (keys %ausgabedatei)
#
#    # Aufbau der Einzeltitel
#    my %sortlist;
#    if ($depot eq 'Summe') {
#      # Depotuebersicht
#      foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
#        if ($pos ne 'Summe' && (split(' ', $pos))[0] ne '1') {
#          $sortlist{uc($pos)} = $pos;
#        }
#      }
#    } else {
#      # Einzeldepots
#      foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
#        $symbol = (split(' ', $pos))[0];
#        if ($self->{Kurs}{$symbol}{name}) {
#          $sortlist{uc($self->{Kurs}{$symbol}{name} . $pos)} = $pos;
#        }
#      }
#    }
#
#    foreach my $key (sort(keys %sortlist)) {
#      my $pos = $sortlist{$key};
#      if ($pos ne 'Summe') {
#        $bg = $bg eq $self->{BG}->{normal} ? $self->{BG}->{invers} : $self->{BG}->{normal};
#        if ($depot eq 'Summe') {
#          $format = "Depotname|$pos|";
#          $format .= 'P|' . color($fg{$self->{Portofolios}{$depot}{$pos}{Rating}{Portion}}) . '|';
#        } else {
#          $symbol = (split(' ', $pos))[0];
#          $format = "Symbollangname|" . (split('\|', $symbol))[0] . "|";
#          # Durchfuehren der Einzelwertformatierung und
#          # Einsammeln aller allgemeinen Einzelwertattribute
#          foreach my $flag (keys %{$self->{Kurs}{$symbol}}) {
#            if (defined($flag) && defined($symbol) && defined($self->{Kurs}{$symbol}{$flag})) {$format .= "$flag|$self->{Kurs}{$symbol}{$flag}|"};
#          }
#          $format .= 'P|' . color($fg{$self->{Portofolios}{$depot}{$pos}{Rating}{Portion}}) . '|';
#          $format .= 'Y|' . color($fg{$self->{Portofolios}{$depot}{$pos}{Rating}{Dividende}}) . '|';
#          $format .= 'L|' . color($fg{$self->{Portofolios}{$depot}{$pos}{Rating}{Last_Trade}}) . '|';
#        } ## end else [ if ($depot eq 'Summe')]
#        # Einsammeln aller depotspezifischen Einzelwertattribute
#        foreach (keys %{$self->{Portofolios}{$depot}{$pos}}) {
#          $format .= "$_|$self->{Portofolios}{$depot}{$pos}{$_}|";
#        }
#        $format .= 'C|' . color($self->{FG}->{neutral}) . '|';
#        $format .= 'W|' . color($fg{$self->{Portofolios}{$depot}{$pos}{Rating}{Win}}) . '|';
#        $format .= 'D|' . color($fg{$self->{Portofolios}{$depot}{$pos}{Rating}{Win_Day}}) . '|';
#      } ## end if ($pos ne 'Summe')
#
#      foreach my $ausgabe (keys %ausgabedatei) {
#        my $ausgabeType = ($depot eq 'Summe') ? "${ausgabe}_SBody" : "${ausgabe}_Body";
#        if (Configuration->config('Ausgabeformat', $ausgabeType)) {
#          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, Utils::extendString(Configuration->config('Ausgabeformat', $ausgabeType), $format));
#        }
#      }
#    } ## end foreach (sort(keys %sortlist...))
#  } ## end foreach my $depot (keys %{$self...})
#
#  # Ausgabedateien schreiben
#  foreach my $ausgabe (keys %{$self->{Ausgabe}}) {
#    foreach my $depot (keys %{$self->{Ausgabe}{$ausgabe}}) {
#      Trace->Trc('I', 2, 0x02501, $depot, Utils::extendString($ausgabedatei{$ausgabe}, "Depot|$depot"));
#      if (!CmdLine->argument(0) || $depot ne 'Summe') {
#
#        # Anlegen
#        #Trace->Log($ausgabe, Utils::extendString($ausgabedatei{$ausgabe} . '.tmp', "Depot|$depot")) or Trace->Exit(1, 0, 0x00010, $ausgabedatei{$ausgabe}, "Depot|$depot");
#        my $dateiname = Utils::extendString($ausgabedatei{$ausgabe}, "Depot|$depot");
#        open(DEPOT, '> ' . $dateiname . '.tmp');
#        while (my $line = shift(@{$self->{Ausgabe}{$ausgabe}{$depot}})) {
#          print DEPOT "$line\n";
#        }
#
#        # Schliessen und Umbenennen
#        #$self->TerminateLog($ausgabe);
#        close(DEPOT);
#        move($dateiname . '.tmp', $dateiname);
#      } ## end if (!CmdLine->argument...)
#    } ## end foreach my $depot (keys %{$self...})
#  } ## end foreach my $ausgabe (keys %...)
#
#  # Kursinfos löschen
#  if (exists($self->{VarFile})) {
#    eval {unlink($self->{VarFile});};
#  }
#
#  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
#  $self->{subroutine} = $merker;
#
#  return $rc;
#} ## end sub schreibe_Ausgabe


1;
