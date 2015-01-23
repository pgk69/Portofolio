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
    if ($default eq '-') {$default = ' '}
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
          if (defined($flag)) {
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
      } else {
        $$hashptr{_aktuell} = 1;
      }
      $$hashptr{_letzter_Abruf} = time;
    } else {
      $$hashptr{_Tradetime} = 0;
    }

    if ($$hashptr{name}) {
      $$hashptr{name} =~ s/\s+N$//
    }

    Trace->Trc('I', 2, 0x02102, $symbol, $$hashptr{name}, $$hashptr{last});

    $/ = $crlfmerker;
  } ## end if (!$$hashptr{_aktuell...})

  # Kursinfos speichern
  if (exists($self->{VarFile})) {
    store $self->{Kurs}, $self->{VarFile};
  }

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

  my $symbol  = shift;
  my $hashptr = shift;
  my $infoptr = shift;
  my $quelle  = shift;

  no autovivification;

  my $rc = 0;

  my %value;
  foreach (keys %{$self->{Flags}}) {
    Trace->Trc('I', 2, 0x02102, $symbol, $_, $$infoptr{$symbol, $_});
    if (defined($$infoptr{$symbol, $_})) {
      $value{$_} = substr($$infoptr{$symbol, $_}, 0, $self->{Flags}{$_}{Laenge})
    } else {
      $value{$_} = $self->{Flags}{$_}{Default};
    }
  }

  my $tradetime = 0;
  if ($value{date} && $value{'time'}) {
    $tradetime = str2time("$value{date} $value{'time'}") || 0;
  }
  
  if (!defined($$hashptr{_tradetime}) || ($tradetime > $$hashptr{_tradetime})) {
    $$hashptr{_aktuell} = 0;

    foreach my $flagname (keys(%value)) {
      $value{$flagname} =~ s/^\s+|\s+$//g;
      if ($self->{Flags}{$flagname}{Faktor}) {
        $$hashptr{$flagname . '_RAW'} = $value{$flagname}
      }
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
    } else {
      $$hashptr{_Tradetime} = 0;
    }

    if ($$hashptr{name}) {
      $$hashptr{name} =~ s/\s+N$//
    }

    if (!defined($$hashptr{symbol}) || ($$hashptr{symbol} eq '')) {
      $$hashptr{symbol} = $symbol;
    }

    if (!defined($$hashptr{exchange}) || ($$hashptr{exchange} eq '')) {
      $$hashptr{exchange} = $quelle;
    }
  }


  Trace->Trc('I', 2, 0x02102, $symbol, $$hashptr{name}, $$hashptr{last});

  $/ = $crlfmerker;

  # Kursinfos speichern
  if (exists($self->{VarFile})) {
    store $self->{Kurs}, $self->{VarFile};
  }

  Trace->Trc('S', 3, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end if (!$$hashptr{_aktuell...})


sub Kurse_ermitteln {
  #################################################################
  # Ermittelt Kurse zu allen PApieren, die in den Depots gefunden 
  # wurden
  # Ergebnis: In der Datenstruktur $self->{Kurse} liegt
  # ein Hash (Portofolios) von Hashes (Position) mit:
  #
  # Pos_Quantity          : Amount of Shares in Position

  # Pos_Buy_Price         : Buying Price of whole Position
  # Share_Buy_Price       : Buying Price for one Share

  #
  # Prozedurnummer 2
  #
  my $self = shift;
  my $singleportofolio = shift || '';

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  # Kurs ermitteln
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
    my $quoter = Finance::Quote->new();
    $quoter->set_currency(EUR);  # Set default currency.
    
    # Array ueber alle Symbole erstellen
    my @markets = split(' ', Configuration->config('Stockservice', 'Markets'));
    if (!@markets) {@markets = (qw/vwd europe/)};
    my @symbols;
    my %symbolhash;
    foreach (keys %{$self->{Kurs}}) {
      foreach my $symbol (split('\|', $_)) {
        $symbolhash{$symbol} = $_;
        my $waehrung = 'EUR' . $self->{Kurs}{$_}{_Waehrung} . '=X';
        $symbolhash{$waehrung} = $waehrung;
        $self->{Kurs}{$waehrung}{_Waehrung} = $self->{Kurs}{$_}{_Waehrung};
        $self->{Kurs}{$waehrung}{_last}     = 0;
      }
    }
    foreach (sort keys %symbolhash) {
      push(@symbols, $_) if (!$self->{Kurs}{$symbolhash{$_}}{_aktuell})
    }

    foreach my $quelle (@markets) {
      if (scalar(@symbols)) {
        Trace->Trc('I', 2, 0x02101, $quelle, join(' ', @symbols));
        my %info = $quoter->fetch($quelle, @symbols);

        # Ergebnisse auswerten und @symbol aktualisieren
        foreach my $symbol (@symbols) {
          next unless $info{$symbol,"success"}; # Skip failures.
          next unless $info{$symbol,"last"} ne '0,00'; # Skip failures.
          $self->_webabruf_QUOTE($symbol, $self->{Kurs}{$symbolhash{$symbol}}, \%info, $quelle);
        }
        # Remove the symbols I already got a current info
        undef %info;
        undef @symbols;
        foreach (sort keys %symbolhash) {
          push(@symbols, $_) if (!$self->{Kurs}{$symbolhash{$_}}{_aktuell})
        }
      }
    }
    foreach my $symbol (keys %{$self->{Kurs}}) {
      if ($self->{Kurs}{$symbol}{_Waehrung} ne 'EUR') {
        foreach my $EUR (keys %{$self->{Kurs}{$symbol}}) {
          if (defined($self->{Kurs}{$symbol}{$EUR . '_RAW'})) {
            my $xsymbol = 'EUR' . $self->{Kurs}{$symbol}{_Waehrung} . '=X';
            if ($self->{Kurs}{$xsymbol}{last}) {
              $self->{Kurs}{$symbol}{$EUR . '_RAW'} = $self->{Kurs}{$symbol}{$EUR} * $self->{Kurs}{$xsymbol}{last}
            }
          }
        }
      }
    }
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub Kurse_ermitteln


sub lese_Portofolios {
  #################################################################
  # Einlesen der Portofolios mit Inhalten
  # Ergebnis: In der Datenstruktur $self->{Portofolios} liegt
  # Hash (Portofolios) von Hashes (Aktienposition) mit:
  #
  # Pos_Quantity          : Amount of Shares in Position

  # Pos_Buy_Price         : Buying Price of whole Position
  # Share_Buy_Price       : Buying Price for one Share

  #
  # Prozedurnummer 3
  #
  my $self = shift;
  my $singleportofolio = shift || '';

  my $merker = $self->{subroutine};
  $self->{subroutine} = (caller(0))[3];
  Trace->Trc('S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei});

  my $rc = 0;

  # Sammel alle Konfigdateien ein
  my @portofolio;
  my %portofolios;
  my %portofoliofiles = Configuration->config('Eingabedatei');
  foreach (keys(%portofoliofiles)) {
    Trace->Trc('I', 2, 0x02201, $_);
    my %localportofolios;
    if (-r $portofoliofiles{$_} && tie(%localportofolios, 'Config::IniFiles', (-file => $portofoliofiles{$_}))) {
      foreach my $localportofolioname (keys %localportofolios) {
        # Sammle alle Portofolios oder Watchlists inklusive Namen aus den Konfigurationsfiles
        if ($localportofolioname =~ /^(Portofolio|Watchlist) .*$/) {
          Trace->Trc('I', 2, 0x02202, $_, $localportofolioname);
          if (defined($portofolios{$localportofolioname})) {
            # Portofolio oder Teile davon wurden schonmal vorher definiert
            foreach my $pos (keys %{$localportofolios{$localportofolioname}}) {
              Trace->Trc('I', 2, 0x02203, $_, $localportofolioname, $pos);
              if (defined($portofolios{$localportofolioname}{$pos})) {
                # Die spezielle Position ist schon definiert -> anhaengen -> POSITION WIRD ZU EINEM ARRAY, FALLS SIE NOCH KEINER IST
                my @dummy;
                if (ref($portofolios{$localportofolioname}{$pos})) {
                  @dummy = @{$portofolios{$localportofolioname}{$pos}};
                } else {
                  push(@dummy, $portofolios{$localportofolioname}{$pos});
                }
                if (ref($localportofolios{$localportofolioname}{$pos})) {
                  push(@dummy, @{$localportofolios{$localportofolioname}{$pos}});
                } else {
                  # Falls die Position nur einmal vorkommt schieb das Element in den Array
                  push(@dummy, $localportofolios{$localportofolioname}{$pos});
                }
                $portofolios{$localportofolioname}{$pos} = \@dummy;
              } else {
                # Die spezielle Position ist nocht nicht definiert -> einfach uebernehmen
                $portofolios{$localportofolioname}{$pos} = $localportofolios{$localportofolioname}{$pos};
              }
            } ## end foreach my $pos (keys %{$localportofolios...})
          } else {
            $portofolios{$localportofolioname} = $localportofolios{$localportofolioname};
            push(@portofolio, $localportofolioname);
          }
        } ## end if ($localportofolioname...)
      } ## end foreach my $localportofolioname...
    } ## end if (-r $portofoliofiles...)
  } ## end foreach (keys(%portofoliofiles...))

  if ($singleportofolio) {
    undef(@portofolio);
    push(@portofolio, $$singleportofolio);
    $self->{Gesamtliste} = undef;
  }

  # Ggf. Gesamtliste anlegen
  if ($self->{Gesamtliste}) {
    my %gesamtliste;
    foreach my $portofolioname (%portofolios) {
      if ($portofolioname =~ /^Portofolio .*$/) {
        foreach my $pos (keys %{$portofolios{$portofolioname}}) {
          Trace->Trc('I', 2, 0x02203, $_, $portofolioname, $pos);
          if (defined($gesamtliste{$pos})) {
            # Die spezielle Position ist schon definiert -> anhaengen -> POSITION WIRD ZU EINEM ARRAY, FALLS SIE NOCH KEINER IST
            my @dummy;
            if (ref($gesamtliste{$pos})) {
              @dummy = @{$gesamtliste{$pos}};
            } else {
              push(@dummy, $gesamtliste{$pos});
            }
            if (ref($portofolios{$portofolioname}{$pos})) {
              push(@dummy, @{$portofolios{$portofolioname}{$pos}});
            } else {
              # Falls die Position nur einmal vorkommt schieb das Element in den Array
              push(@dummy, $portofolios{$portofolioname}{$pos});
            }
            $gesamtliste{$pos} = \@dummy;
          } else {
            # Die spezielle Position ist nocht nicht definiert -> einfach uebernehmen
            $gesamtliste{$pos} = $portofolios{$portofolioname}{$pos};
          }
        }
      }
    }
    push(@portofolio, "Watchlist $self->{Gesamtliste}");
    $portofolios{"Watchlist $self->{Gesamtliste}"} = \%gesamtliste;
  }

  if (exists($self->{VarFile})) {
    eval {$self->{Kurs} = retrieve($self->{VarFile});};
  }

  foreach my $portofolioname (sort(@portofolio)) {
    # Fuer jedes gefundene Protofolio/jede Watchlist, der/die Elemente (Aktienpositionen) enthaelt
    if (scalar keys %{$portofolios{$portofolioname}}) {
      foreach my $pos (keys %{$portofolios{$portofolioname}}) {
        Trace->Trc('I', 4, 0x02204, $portofolioname, $pos, $portofolios{$portofolioname}{$pos});
        my @stockattributearray;
        if (ref($portofolios{$portofolioname}{$pos})) {
          # Falls die Position mehrfach vorkommt sammel alle Vorkommen in einem Array
          @stockattributearray = @{$portofolios{$portofolioname}{$pos}};
        } else {
          # Falls die Position nur einmal vorkommt schieb das Element in den Array
          push(@stockattributearray, $portofolios{$portofolioname}{$pos});
        }
        if ($self->{SumPos} && $#stockattributearray) {
          # Falls gewuenscht, Aufsummieren aller Einzelpositionen so, dass am Ende nur noch eine Arrayelement mit allen aufsummierten Positionen uebrig bleibt
          my ($Currency, $Buy_Price, $Quantity, $Dividende, $Branche, $Kurs) = ('', 0, 0, 0, '', 0);
          while ($#stockattributearray >= 0) {
            my @stockattribute = split('\|', shift(@stockattributearray));
            Trace->Trc('I', 3, 0x02205, $portofolioname, $pos, $stockattribute[0] || '', $stockattribute[1] || '', $stockattribute[2] || '', $stockattribute[3] || '', $stockattribute[4] || '');
            $Currency = $stockattribute[0] || $Currency || 'EUR';
            $Quantity  += $stockattribute[1] || 0;
            $Buy_Price += $stockattribute[2] || 0;
            $Dividende = $stockattribute[3] || $Dividende || 0;
            $Branche   = $stockattribute[4] || $Branche   || '';
            $Kurs      = $stockattribute[5] || $Kurs      || 0;
          }
          $stockattributearray[0] = join('|', ($Currency, $Quantity, $Buy_Price, $Dividende, $Branche, $Kurs));
          Trace->Trc('I', 2, 0x02206, $portofolioname, $pos, $Quantity);
        } ## end if ($self->{SumPos} &&...)
        my $poscount = 0;
        foreach my $exg_anz_prz (@stockattributearray) {
          my @stockattribute = split('\|', $exg_anz_prz);
          $poscount++;
          my $ppos = "$pos $poscount";
          $self->{Kurs}{$pos}{_Waehrung}                              = $stockattribute[0] || 'EUR';
          $self->{Kurs}{$pos}{_last}                                  = 0;
          $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Quantity}  = $stockattribute[1];
          $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Buy_Price} = $stockattribute[2];
          $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Dividende} = $stockattribute[3] || 0;
          $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Branche}   = $stockattribute[4] || '';
          $self->{Kurs}{$pos}{_last}                                  = $stockattribute[5] if (defined($stockattribute[5]) && $stockattribute[5] =~ m/^([-+]?)([0-9]*).*$/);
          
          if (looks_like_number($self->{Portofolios}{$portofolioname}{$ppos}{Pos_Quantity}) && 
              $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Quantity} && 
              looks_like_number($self->{Portofolios}{$portofolioname}{$ppos}{Pos_Buy_Price})) {
            $self->{Portofolios}{$portofolioname}{$ppos}{Share_Buy_Price} = $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Buy_Price} / $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Quantity};
          } else {
            Trace->Trc('I', 2, 0x0220f, $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Quantity}, looks_like_number($self->{Portofolios}{$portofolioname}{$ppos}{Pos_Quantity}), $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Buy_Price}, looks_like_number($self->{Portofolios}{$portofolioname}{$ppos}{Pos_Buy_Price}));
            $self->{Portofolios}{$portofolioname}{$ppos}{Share_Buy_Price} = '';
          }
        } ## end foreach my $exg_anz_prz (@stockattributearray)
      } ## end foreach my $pos (keys %{$portofolios...})
    } ## end if (scalar keys %{$portofolios...})
  } ## end foreach my $portofolioname ...

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub lese_Portofolios


sub analysiere_Portofolios {
  #################################################################
  # Ermittelt Kennzahlen zu den Portofolios
  # Ergebnis: In der Datenstruktur $self->{Portofolios} liegt
  # Hash (Portofolios) von Hashes (Aktienposition) mit zusaetzlich:
  # zu:
  #
  # Pos_Quantity           : Amount of Shares in Position
  # Pos_Buy_Price          : Buying Price of whole Position
  # Share_Buy_Price        : Buying Price for one Share
  #

  # Pos_Price              : Current Price of whole Position
  # Pos_Price_EUR          : Current Price of whole Position in EUR
  # Pos_Day_Change         : Daily Change of whole Position
  # Pos_Day_Change_Percent : Daily Change of whole Position in Percent
  # Pos_Change             : Total Change of whole Position
  # Pos_Change_Percent     : Total Change of whole Position in Percent

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
  
  sub spf ($$) {
    my $format = $_[0];
    my $text   = $_[1] || 0;

    my $len = 5;
    if ($format =~ m/^%([-+]?)([0-9]*).*$/) {
      $len = $2;
      $len++ if $1 eq '+';
      $len = '%' . $len . 's';
    }
    $text = sprintf($format, $text);
    $text = reverse $text;
    $text =~ s:\.:,:g; # deutsches Komma als Dezimaltrenner
    $text =~ s/(\d\d\d)(?=\d)(?!\d*,)/$1./g;
    $text = scalar reverse $text;
    $text =~ s/^\s+//;
    $text =~ s/\s+$//;
    $text = sprintf($len, $text);
    return $text;
  }

  my $rc = 0;

  my %summe;
  POSIX::setlocale(&POSIX::LC_ALL, 'de_DE');
  foreach my $depot (keys %{$self->{Portofolios}}) {
    foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
      if (my $symbol = (split(' ', $pos))[0]) {
        if ($self->{Kurs}{$symbol}{last}) {

          # Werte ermitteln
          $self->{Portofolios}{$depot}{$pos}{Pos_Price}              = $self->{Portofolios}{$depot}{$pos}{Pos_Quantity} * $self->{Kurs}{$symbol}{last} || 0;
          $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change}         = $self->{Portofolios}{$depot}{$pos}{Pos_Quantity} * $self->{Kurs}{$symbol}{net} || 0;
          $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change_Percent} = $self->{Kurs}{$symbol}{p_change};
          $self->{Portofolios}{$depot}{$pos}{Pos_Change}             = $self->{Portofolios}{$depot}{$pos}{Pos_Price} - $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price};
          if ($self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price}) {
            $self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent}   = 100 * $self->{Portofolios}{$depot}{$pos}{Pos_Change} / $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price};
          } else {
            $self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent}   = 0;
          }
          $self->{Portofolios}{$depot}{$pos}{Pos_Div_Summe}          = $self->{Portofolios}{$depot}{$pos}{Pos_Quantity} * $self->{Kurs}{$symbol}{div};
          $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Percent}  = $self->{Kurs}{$symbol}{last} ? 100 * $self->{Portofolios}{$depot}{$pos}{Pos_Dividende} / $self->{Kurs}{$symbol}{last} : 0;
          $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Summe}    = $self->{Portofolios}{$depot}{$pos}{Pos_Price} * $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Percent} / 100;

          # Aufsummieren Portofolio und Gesamt
          $summe{$depot}{Pos_Buy_Price}       += $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price};
          $summe{$depot}{Pos_Price}           += $self->{Portofolios}{$depot}{$pos}{Pos_Price};
          $summe{$depot}{Pos_Day_Change}      += $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change};
          $summe{$depot}{Pos_Dividende_Summe} += $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Summe};

          # In der Aufsummierung aller Depots werden nur Portofolios beruecksichtigt,
          # keine Watchlists
          if ($depot =~ /^Portofolio/) {
            $summe{Summe}{Pos_Buy_Price}       += $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price};
            $summe{Summe}{Pos_Price}           += $self->{Portofolios}{$depot}{$pos}{Pos_Price};
            $summe{Summe}{Pos_Day_Change}      += $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change};
            $summe{Summe}{Pos_Dividende_Summe} += $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Summe};
          }
        } ## end if ($self->{Kurs}{$symbol...})
      } ## end if (my $symbol = (split...))
    } ## end foreach my $pos (keys %{$self...})

    foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
      if ($summe{$depot}{Pos_Price} && $self->{Kurs}{(split(' ', $pos))[0]}{last}) {
        # Werte formatieren
        $self->{Portofolios}{$depot}{$pos}{Pos_Anteil}            = spf('%5.2f',  100 * $self->{Portofolios}{$depot}{$pos}{Pos_Price} / $summe{$depot}{Pos_Price});
        $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price}         = spf('%9.2f',  $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price});
        $self->{Portofolios}{$depot}{$pos}{Pos_Dividende}         = spf('%5.2f',  $self->{Portofolios}{$depot}{$pos}{Pos_Dividende});
        $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Percent} = spf('%5.2f',  $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Percent});
        $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Summe}   = spf('%8.2f',  $self->{Portofolios}{$depot}{$pos}{Pos_Dividende_Summe});
        $self->{Portofolios}{$depot}{$pos}{Pos_Price}             = spf('%9.2f',  $self->{Portofolios}{$depot}{$pos}{Pos_Price});
        $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change}        = spf('%+8.2f', $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change});
        $self->{Portofolios}{$depot}{$pos}{Pos_Change}            = spf('%+9.2f', $self->{Portofolios}{$depot}{$pos}{Pos_Change});
        $self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent}    = spf('%+7.2f', $self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent});
        $self->{Portofolios}{$depot}{$pos}{Pos_Div_Summe}         = spf('%8.2f',  $self->{Portofolios}{$depot}{$pos}{Pos_Div_Summe});
        $self->{Portofolios}{$depot}{$pos}{Share_Buy_Price}       = spf('%7.2f',  $self->{Portofolios}{$depot}{$pos}{Share_Buy_Price});
      } ## end if ($summe{$depot}{Pos_Price...})
    } ## end foreach my $pos (keys %{$self...})
  } ## end foreach my $depot (keys %{$self...})

  foreach my $depot (keys %summe) {
    if ($depot ne 'Summe') {
      if ($depot =~ /^Watchlist/) {
        $summe{$depot}{Pos_Anteil} = ' ';
      } else {
        $summe{$depot}{Pos_Anteil} = spf('%5.2f', 100 * $summe{$depot}{Pos_Price} / $summe{Summe}{Pos_Price});
      }

      # Portofoliosummen ermitteln und formatieren
      $summe{$depot}{Pos_Day_Change_Percent} = spf('%+5.2f',  100 * $summe{$depot}{Pos_Day_Change} / ($summe{$depot}{Pos_Price} - $summe{$depot}{Pos_Day_Change}));
      $summe{$depot}{Pos_Day_Change}         = spf('%+10.2f', $summe{$depot}{Pos_Day_Change});
      $summe{$depot}{Pos_Change_Percent}     = spf('%+6.2f',  100 * ($summe{$depot}{Pos_Price} / $summe{$depot}{Pos_Buy_Price} - 1));
      $summe{$depot}{Pos_Change}             = spf('%+11.2f', $summe{$depot}{Pos_Price} - $summe{$depot}{Pos_Buy_Price});
      $summe{$depot}{Pos_Dividende_Percent}  = spf('%4.2f',   100 * ($summe{$depot}{Pos_Dividende_Summe} / $summe{$depot}{Pos_Price}));
      $summe{$depot}{Pos_Dividende_Summe}    = spf('%9.2f',   $summe{$depot}{Pos_Dividende_Summe});
      $summe{$depot}{Pos_Price}              = spf('%12.2f',  $summe{$depot}{Pos_Price});
      $summe{$depot}{Pos_Buy_Price}          = spf('%12.2f',  $summe{$depot}{Pos_Buy_Price});
    } ## end if ($depot ne 'Summe')
  } ## end foreach my $depot (keys %summe)
  if (defined($summe{Summe})) {
    $summe{Summe}{Pos_Anteil}             = spf('%5.2f',   100);
    $summe{Summe}{Pos_Day_Change_Percent} = spf('%+5.2f',  100 * $summe{Summe}{Pos_Day_Change} / ($summe{Summe}{Pos_Price} - $summe{Summe}{Pos_Day_Change}));
    $summe{Summe}{Pos_Day_Change}         = spf('%+10.2f', $summe{Summe}{Pos_Day_Change});
    $summe{Summe}{Pos_Change_Percent}     = spf('%+6.2f',  100 * ($summe{Summe}{Pos_Price} / $summe{Summe}{Pos_Buy_Price} - 1));
    $summe{Summe}{Pos_Change}             = spf('%+11.2f', $summe{Summe}{Pos_Price} - $summe{Summe}{Pos_Buy_Price});
    $summe{Summe}{Pos_Dividende_Percent}  = spf('%4.2f',   100 * ($summe{Summe}{Pos_Dividende_Summe} / $summe{Summe}{Pos_Price}));
    $summe{Summe}{Pos_Dividende_Summe}    = spf('%9.2f',   $summe{Summe}{Pos_Dividende_Summe});
    $summe{Summe}{Pos_Price}              = spf('%12.2f',  $summe{Summe}{Pos_Price});
    $summe{Summe}{Pos_Buy_Price}          = spf('%12.2f',  $summe{Summe}{Pos_Buy_Price});
    $self->{Portofolios}{Summe} = \%summe;
  } ## end if (defined($summe{Summe...}))

  foreach my $symbol (keys %{$self->{Kurs}}) {
    foreach my $flagname (keys %{$self->{Kurs}{$symbol}}) {
      if ($self->{Flags}{$flagname}{Format}) {
        $self->{Kurs}{$symbol}{$flagname} = sprintf($self->{Flags}{$flagname}{Format}, $self->{Kurs}{$symbol}{$flagname});
      }
    }
  }

  Trace->Trc('S', 1, 0x00002, $self->{subroutine});
  $self->{subroutine} = $merker;

  return $rc;
} ## end sub analysiere_Portofolios


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
  my ($dateiname, $format, $symbol);
  our $bg = 'on_black';


  sub colorize {

    # my $decider = (split(/,/, shift || 0))[0];
    (my $decider = shift || 0) =~ s/^\s+|\s+$//g;
    $decider =~ s:,:\.:;
    my ($m2, $m1, $p1, $p2) = (shift || 0, shift || 0, shift || 0, shift || 0);

    # my ($m2, $m1, $p1, $p2) = (-2, -1, 1, 2);
    $decider = 0 if (!$decider);
    my $rc;
    if ($m1 < $p1) {
      $rc = color('green ' . $bg) if ($decider >= $p2);
      $rc = color('cyan ' . $bg)
        if ($decider >= $p1 && $decider < $p2);
      $rc = color('clear ' . $bg)
        if ($decider > $m1 && $decider < $p1);
      $rc = color('magenta ' . $bg)
        if ($decider <= $m1 && $decider > $m2);
      $rc = color('red ' . $bg) if ($decider <= $m2);
    } elsif ($m1 == $p1) {
      $rc = color('red ' . $bg) if ($decider >= $p2);
      $rc = color('cyan ' . $bg)
        if ($decider >= $p1 && $decider < $p2);
      $rc = color('green ' . $bg) if ($decider < $p1);
    } else {
      $rc = color('green ' . $bg) if ($decider <= $p2);
      $rc = color('cyan ' . $bg)
        if ($decider <= $p1 && $decider > $p2);
      $rc = color('clear ' . $bg)
        if ($decider < $m1 && $decider > $p1);
      $rc = color('magenta ' . $bg)
        if ($decider >= $m1 && $decider < $m2);
      $rc = color('red ' . $bg) if ($decider >= $m2);
    } ## end else [ if ($m1 < $p1) ]

    return $rc;
  } ## end sub colorize

  my %ausgabedatei = Configuration->config('Ausgabedatei');
  foreach my $depot (keys %{$self->{Portofolios}}) {
    $bg     = 'on_black';
    $format = "Portofolio|$depot|";
    foreach my $pos (keys %{$self->{Portofolios}{Summe}{$depot}}) {
      $format .= "$pos|$self->{Portofolios}{Summe}{$depot}{$pos}|";
    }

    # Ausgabe der Summeninformation und der Kopfzeile
    # Farbdefinition:
    # CC : Ruecksetzen der Farbe
    # CK : Aenderung seit Kauf
    # CT : Tagesaenderung
    # CA : Anteil am Gesamtvermoegen
    # CD : Dividende
    # CL : Letzter Kurs veraltet
    $format .= 'CC|' . color('clear') . '|';
    $format .= 'CK|' . colorize($self->{Portofolios}{Summe}{$depot}{Pos_Change_Percent} || 0, -10, -5, 5, 10) . '|';
    $format .= 'CT|' . colorize($self->{Portofolios}{Summe}{$depot}{Pos_Day_Change_Percent} || 0, -2, -1, 1, 2) . '|';
    $format .= 'CA|' . colorize($self->{Portofolios}{Summe}{$depot}{Pos_Anteil} || 0, 50, 40, 30, 20) . '|';

    # Ausgabe der Header fuer TXT und CSV puffern
    foreach my $ausgabe (keys %ausgabedatei) {
      if ($self->{Portofolios}{Summe}{$depot}) {
        # Unterscheidung Depotuebersicht oder Einzeldepot
        my $summe = ($depot eq 'Summe') ? '_S' : '_';
        if (Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Head')) {
          my $line = (Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Trenn')) ? Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Trenn') . "\n" : "\n";
          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, Utils::extendString(Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Head'), $format) . "\n");
          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, $line);
          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, Utils::extendString(Configuration->config('Ausgabeformat', $ausgabe . $summe . 'Col'),  $format) . "\n");
          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, $line);
        }
      } ## end if ($self->{Portofolios...})
    } ## end foreach my $ausgabe (keys %ausgabedatei)

    # Aufbau der Einzeltitel
    my %sortlist;
    if ($depot eq 'Summe') {
      # Depotuebersicht
      foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
        if ($pos ne 'Summe' && (split(' ', $pos))[0] ne '1') {
          $sortlist{uc($pos)} = $pos;
        }
      }
    } else {
      # Einzeldepots
      foreach my $pos (keys %{$self->{Portofolios}{$depot}}) {
        $symbol = (split(' ', $pos))[0];
        if ($self->{Kurs}{$symbol}{name}) {
          $sortlist{uc($self->{Kurs}{$symbol}{name} . $pos)} = $pos;
        }
      }
    }

    foreach my $key (sort(keys %sortlist)) {
      my $pos = $sortlist{$key};
      if ($pos ne 'Summe') {
        $bg = $bg eq 'on_black' ? 'on_blue' : 'on_black';
        if ($depot eq 'Summe') {
          $format = "Depotname|$pos|";
          $format .= 'CA|' . colorize($self->{Portofolios}{$depot}{$pos}{Pos_Anteil} || 0, 50, 40, 30, 20) . '|';
        } else {
          $symbol = (split(' ', $pos))[0];
          $format = "Symbollangname|" . (split('\|', $symbol))[0] . "|";
          # Durchfuehren der Einzelwertformatierung und
          # Einsammeln aller allgemeinen Einzelwertattribute
          foreach my $flag (keys %{$self->{Kurs}{$symbol}}) {
            if (defined($flag) && defined($symbol) && defined($self->{Kurs}{$symbol}{$flag})) {$format .= "$flag|$self->{Kurs}{$symbol}{$flag}|"};
          }
          $format .= 'CA|' . colorize($self->{Portofolios}{$depot}{$pos}{Pos_Anteil} || 0, 5, 4, 3, 2) . '|';
          $format .= 'CD|' . colorize($self->{Kurs}{$symbol}{div_yield} || 0, 1, 2, 3, 4) . '|';
          $format .= 'CL|' . colorize($self->{Kurs}{$symbol}{_lastTrade}, 3, 2, 1, 0) . '|';
        } ## end else [ if ($depot eq 'Summe')]
        # Einsammeln aller depotspezifischen Einzelwertattribute
        foreach (keys %{$self->{Portofolios}{$depot}{$pos}}) {
          $format .= "$_|$self->{Portofolios}{$depot}{$pos}{$_}|";
        }
        $format .= 'CC|' . color('clear') . '|';
        $format .= 'CK|' . colorize($self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent} || 0, -10, -5, 5, 10) . '|';
        $format .= 'CT|' . colorize($self->{Portofolios}{$depot}{$pos}{Pos_Day_Change_Percent} || 0, -2, -1, 1, 2) . '|';
      } ## end if ($pos ne 'Summe')

      foreach my $ausgabe (keys %ausgabedatei) {
        my $ausgabeType = ($depot eq 'Summe') ? "${ausgabe}_SBody" : "${ausgabe}_Body";
        if (Configuration->config('Ausgabeformat', $ausgabeType)) {
          push(@{$self->{Ausgabe}{$ausgabe}{$depot}}, Utils::extendString(Configuration->config('Ausgabeformat', $ausgabeType), $format) . "\n");
        }
      }
    } ## end foreach (sort(keys %sortlist...))
  } ## end foreach my $depot (keys %{$self...})

  # Ausgabedateien schreiben
  foreach my $ausgabe (keys %{$self->{Ausgabe}}) {
    foreach my $depot (keys %{$self->{Ausgabe}{$ausgabe}}) {
      Trace->Trc('I', 2, 0x02501, $depot, Utils::extendString($ausgabedatei{$ausgabe}, "Depot|$depot"));
      if (!CmdLine->argument(0) || $depot ne 'Summe') {

        # Anlegen
        #Trace->Log($ausgabe, Utils::extendString($ausgabedatei{$ausgabe} . '.tmp', "Depot|$depot")) or Trace->Exit(1, 0, 0x00010, $ausgabedatei{$ausgabe}, "Depot|$depot");
        my $dateiname = Utils::extendString($ausgabedatei{$ausgabe}, "Depot|$depot");
        open(DEPOT, '> ' . $dateiname . '.tmp');
        while (my $line = shift(@{$self->{Ausgabe}{$ausgabe}{$depot}})) {
          print DEPOT $line;
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

1;
