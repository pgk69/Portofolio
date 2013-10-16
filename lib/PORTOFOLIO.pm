package PORTOFOLIO;

#-------------------------------------------------------------------------------
# Letzte Aenderung:     $Date: 2012-06-04 15:53:45 +0200 (Mo, 04 Jun 2012) $
#                       $Revision: 853 $
#                       $Author: xck90n1 $
#
# Aufgabe:				- Ausfuehrbarer Code von programm.pl
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

( $VERSION = SVN_ID ) =~ s/^(.*\$Revision: )([0-9]*)(.*)$/1.0 R$2/ms;
$SVN      = $VERSION . ' ' . SVN_ID;
$OVERSION = $VERSION;

use base 'Exporter';

our @EXPORT    = ();
our @EXPORT_OK = ();

use vars @EXPORT, @EXPORT_OK;

use vars qw(@ISA);
@ISA = qw(DBAccess Trace Configuration CmdLine);

#
# Module
#
#use Data::Dumper;
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

#use File::stat();
#use File::Basename;
#use File::Path qw(mkpath);
#use IO::Compress::Gzip qw(gzip $GzipError);
# use MIME::Lite;

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

    $VERSION = $pversion if ( defined($pversion) );

    return wantarray() ? ( $VERSION, $OVERSION ) : $VERSION;
}

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
}

sub _init {
    #################################################################
    #   Initialisiert ein neues Objekt
    my $self = shift;
    my @args = @_;

    no autovivification;

    $self->{Startzeit} = time();

    if ( $self->config( 'Prg', 'Plugin' ) ) {

        # refs ausschalten wg. dyn. Proceduren
        no strict 'refs';
        my %plugin = ();

        # Bearbeiten aller Erweiterungsmodule die in der INI-Datei
        # in Sektion [Prg] unter "Plugin =" definiert sind
        foreach ( split( ' ', $self->config( 'Prg', 'Plugin' ) ) ) {

            # Falls ein Modul existiert
            if ( -e "$self->{Pfad}/plugins/${_}.pm" ) {

                # Einbinden des Moduls
                require $_ . '.pm';
                $_->import();

                # Initialisieren des Moduls, falls es eine eigene Sektion
                # [<Modulname>] fuer das Module in der INI-Datei gibt
                $plugin{$_} =
                  eval { $_->new( $self->config( 'Plugin ' . $_ ) ) };
                eval {
                        $plugin{$_}
                      ? $plugin{$_}->DESTROY
                      : ( $_ . '::DESTROY' )->()
                      if ( $self->option('erase') );
                };
            }
        }
        use strict;
    }

    # Module::Refresh->refresh;

    # Test der benoetigten INI-Variablen
    # DB-Zugriff

    # Web-Abfrage
    $self->Exit( 1, 0, 0x0f009, 'Stockservice', 'URL' )
      if ( !defined( $self->config( 'Stockservice', 'URL' ) ) );
    $self->Exit( 1, 0, 0x0f009, 'Stockinfos' )
      if ( !defined( $self->config('Stockinfos') ) );

    # Ergebnisausgabe und Sicherung
    if ( $self->config( 'Prg', 'LockFile' ) ) {
        $self->{LockFile} = Utils::extendString(
            $self->config( 'Prg', 'LockFile' ),
            "BIN|$Bin|SCRIPT|" . uc($Script)
        );
        $self->{Lock} = LockFile::Simple->make(
            -max       => 5,
            -delay     => 1,
            -format    => '%f',
            -autoclean => 1,
            -stale     => 1
        );
        my $errtxt;
        $SIG{'__WARN__'} = sub { $errtxt = $_[0] };
        my $lockerg = $self->{Lock}->trylock( $self->{LockFile} );
        undef( $SIG{'__WARN__'} );
        if ( defined($errtxt) ) {
            $errtxt =~ s/^(.*) .+ .+ line [0-9]+.*$/$1/;
            chomp($errtxt);
            $self->Trc( 'S', 1, 0x02000, $errtxt ) if defined($errtxt);
        }
        if ( !$lockerg ) {
            $self->Exit( 0, 1, 0x0a000, $self->prg, $self->{LockFile} );
        }
        else {
            $self->Trc( 'S', 1, 0x02001, $self->{LockFile} );
        }
    }

    # Rücksichern der Variablenwerte
    if ( $self->config( 'Prg', 'VarFile' ) && !$self->option('flush') ) {
        $self->{VarFile} = Utils::extendString(
            $self->config( 'Prg', 'VarFile' ),
            "BIN|$Bin|SCRIPT|" . uc($Script)
        );
    }

    # Positionen aufsummieren
    $self->{SumPos} = $self->config( 'Prg', 'PosAufsummieren' ) ? 1 : 0;

    # Zeitzone festlegen
    my ( $S, $M, $H, $d, $m, $y, $wd, $yd, $sz ) = localtime(time);
    $m  += 1;
    $yd += 1;
    $y  += 1900;
    $self->{TZ} = 'EST';
    my $dlsstart = ( 6 * $y - int( $y / 4 ) - 2 ) % 7 + 8;
    my $dlsend = ( 7 - ( $y * 5 / 4 + 1 ) % 7 );
    if (   ( ( $m > 3 ) && ( $m < 11 ) )
        || ( ( $m == 3 )  && ( $d >= $dlsstart ) )
        || ( ( $m == 11 ) && ( $d < $dlsend ) ) )
    {
        $self->{TZ} = 'EDT';
    }

    # Aufbau der Abfrage-URL
    my %stockInfos = $self->config('Stockinfos');
    my %flags;

    foreach ( keys %stockInfos ) {
        my ( $info, $faktor, @format ) = split( ' ', $stockInfos{$_} );
        $flags{$_} = {
            Flag   => $info,
            Faktor => $faktor || 0,
            Format => join( ' ', @format ) || '%s'
        };
    }

    $self->{Flags} = \%flags;

    if ( my $Testkurs = $self->config( 'Stockservice', 'Testkurs' ) ) {
        $self->{Testkurs} = $Testkurs;
    }

}

sub DESTROY {
    #################################################################
    #     Zerstoert das Objekt an
    my $self = shift;
    my ( $rc, $sig ) = ( 0, 0 );
    $rc  = ( $? >> 8 );
    $sig = $? & 127;
    if ( $@ || $rc != 0 || $sig != 0 ) {
        my ( $routine, $i ) = ( ( caller(0) )[3] . ':', 0 );
        while ( defined( caller( ++$i ) ) ) {
            $routine .=
              ( caller($i) )[3] . '(' . ( caller( $i - 1 ) )[2] . '):';
        }
        $self->Trc( 'S', 1, 0x00007, "$routine $@ $! $?" );
        $self->Log( 'Log', 0x10013, $@, $!, $? );
    }
    for my $parent (@ISA) {
        if ( my $coderef = $self->can( $parent . '::DESTROY' ) ) {
            $self->$coderef();
        }
    }

    # Eigentlich nicht noetig, da -autoclean => 1
    if ( $self->{Lock} ) {
        $self->{Lock}->unlock( $self->{LockFile} );
    }
}

sub _webabruf {
    #################################################################
    # Durchfuehren eines Webabrufs

    #
    # Prozedurnummer 1
    #
    my $self = shift;

    my $merker = $self->{subroutine};
    $self->{subroutine} = ( caller(0) )[3];
    $self->Trc( 'S', 3, 0x00001, $self->{subroutine}, $self->{Eingabedatei} );

    my $symbol  = shift;
    my $hashptr = shift;
    my @flags   = @_;

    no autovivification;

    my $rc = 0;

    if (
        !$$hashptr{_aktuell}
        && ( !$$hashptr{_letzter_Abruf}
            || ( $$hashptr{_letzter_Abruf} < time - 3600 ) )
      )
    {

        push( @flags, ( 'l1', 'd1', 't1' ) );
        my %unique = ();
        $unique{$_} = 0 for @flags;
        @flags = sort( keys %unique );

        $self->Trc( 'I', 2, 0x02101, $symbol, join( '', @flags ) );

        my ( @myFlags, $flag, $stockdata, $value );

        my $webAbruf =
          Utils::extendString( $self->config( 'Stockservice', 'URL' ),
            'STOCK|' . $symbol . '|DATA|' . join( '', @flags ) );
        my $maxtry = $self->config( 'Stockservice', 'Anzahl_Versuche' ) || 10;

        my $crlfmerker = $/;
        $/ = '%';

        $$hashptr{_aktuell} = 0;
        do {
            @myFlags   = @flags;
            $stockdata = get($webAbruf);
            if ( defined($stockdata) && $stockdata !~ 'Missing' ) {

                # die "Failed to get Stock Data" unless defined $stockdata;
                while (
                    $stockdata =~ /\"[\s0]*([^\"]+?)[\s]*\"|[\s0]*([^,\s]+)/g )
                {
                    chomp( $value = $1 ? $1 : $2 );
                    $value = 0 if ( $value eq 'N/A' );
                    $flag = shift(@myFlags);
                    if ( $self->{Flags}{$flag}{Faktor} && $$hashptr{_Waehrung} )
                    {
                        if ( $self->{Flags}{$flag}{Faktor} == 1 ) {
                            my $curxchg = 'EUR' . $$hashptr{_Waehrung} . '=X';

             # Falls der Wechselkurs fuer die Waehrung noch nicht ermittelt ist,
             # versuchen wir das
                            if ( !$self->{Kurs}{$curxchg}{l1} ) {

                                # Default -> Wechselkurs 1
                                $self->{Kurs}{$curxchg}{l1} = 1;
                                if ( $$hashptr{_Waehrung} ne 'EUR' ) {
                                    if ( $$hashptr{_Waehrung} =~ /^[0-9.,]+/ ) {

                   # Keine Standardwaehrung -> Wechselkurs wird direkt angegeben
                                        $self->{Kurs}{$curxchg}{l1} =
                                          $$hashptr{_Waehrung};
                                    }
                                    elsif ( $$hashptr{_Waehrung} =~
                                        /^[A-Za-z][A-Za-z0-9]{2}/ )
                                    {

                                # Standardwaehrung -> Wechselkurs wird ermittelt
                                        $self->_webabruf( $curxchg,
                                            $self->{Kurs}{$curxchg} );
                                    }
                                }
                            }

           # Der Wert in EUR wird nur ermittelt, falls ein Wechselkurs existiert
                            $value = $value / $self->{Kurs}{$curxchg}{l1}
                              if ( $self->{Kurs}{$curxchg}{l1} );
                        }
                        else {
                            $value = $value * $self->{Flags}{$flag}{Faktor};
                        }
                    }
                    $$hashptr{$flag} = $value;
                }
            }
            $maxtry--;
          } until (
            $maxtry <= 0 || ( $$hashptr{l1}
                && $$hashptr{l1} ne 'N/A'
                && $$hashptr{l1} > 0
                && $$hashptr{l1} <= 1000000
                && $$hashptr{l1} != 1 )
          );

        if ( $$hashptr{d1} && $$hashptr{t1} ) {
            my $tradetime =
              str2time( "$$hashptr{d1} $$hashptr{t1}", $self->{TZ} ) || 0;
            $$hashptr{_Tradetime} = time2str( '%d.%m.%y %R', $tradetime );
            $$hashptr{_lastTrade} = abs(
                (
                    str2time( time2str( '%x', time ) ) -
                      str2time( time2str( '%x', $tradetime ) )
                ) / 86400
            );

            if ( $tradetime < str2time( time2str( '%x', time ) ) ) {

                # $$hashptr{_Tradetime} .= '<';
                $$hashptr{_aktuell} = ( $$hashptr{_letzter_Abruf}
                      && $$hashptr{_letzter_Abruf} < time - 3600 ) ? 0 : 1;
            }
            else {
                $$hashptr{_aktuell} = 1;
            }
            $$hashptr{_letzter_Abruf} = time;
        }
        else {
            $$hashptr{_Tradetime} = 0;
        }

        $/ = $crlfmerker;
    }

    # Kursinfos speichern
    if ( exists( $self->{VarFile} ) ) {
        store $self->{Kurs}, $self->{VarFile};
    }

    $self->Trc( 'S', 3, 0x00002, $self->{subroutine} );
    $self->{subroutine} = $merker;

    return $rc;
}

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
    # Prozedurnummer 2
    #
    my $self       = shift;
    my @portofolio = @_;

    my $merker = $self->{subroutine};
    $self->{subroutine} = ( caller(0) )[3];
    $self->Trc( 'S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei} );

    my $rc = 0;

    my %portofolios = $self->config();
    if ( !@portofolio ) {
        foreach ( keys %portofolios ) {
            if ( $_ =~ /^Portofolio (.*)$/ ) { push( @portofolio, $1 ) }
        }
    }

    if ( exists( $self->{VarFile} ) ) {
        eval { $self->{Kurs} = retrieve( $self->{VarFile} ); };
    }

    foreach my $portofolioname ( sort(@portofolio) ) {
        if ( scalar keys %{ $portofolios{"Portofolio $portofolioname"} } ) {
            foreach
              my $pos ( keys %{ $portofolios{"Portofolio $portofolioname"} } )
            {
                my @stockattributearray;
                if ( ref( $portofolios{"Portofolio $portofolioname"}{$pos} ) ) {
                    @stockattributearray =
                      @{ $portofolios{"Portofolio $portofolioname"}{$pos} };
                }
                else {
                    push( @stockattributearray,
                        $portofolios{"Portofolio $portofolioname"}{$pos} );
                }
                if ( $self->{SumPos} && $#stockattributearray ) {
                    my ( $Currency, $Buy_Price, $Quantity ) = ( '', 0, 0 );
                    while ( $#stockattributearray >= 0 ) {
                        my @stockattribute =
                          split( ' ', shift(@stockattributearray) );
                        $Currency = $stockattribute[0];
                        $Quantity  += $stockattribute[1];
                        $Buy_Price += $stockattribute[2];
                    }
                    $stockattributearray[0] =
                      join( ' ', ( $Currency, $Quantity, $Buy_Price ) );
                }
                my $poscount = 0;
                foreach my $exg_anz_prz (@stockattributearray) {
                    my @stockattribute = split( ' ', $exg_anz_prz );
                    $poscount++;
                    my $ppos = "$pos $poscount";
                    $self->{Kurs}{$pos}{_Waehrung} = $stockattribute[0];
                    $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Quantity}
                      = $stockattribute[1];
                    $self->{Portofolios}{$portofolioname}{$ppos}{Pos_Buy_Price}
                      = $stockattribute[2];

                    if (
                        looks_like_number(
                            $self->{Portofolios}{$portofolioname}{$ppos}
                              {Pos_Quantity}
                        )
                        && $self->{Portofolios}{$portofolioname}{$ppos}
                        {Pos_Quantity}
                        && looks_like_number(
                            $self->{Portofolios}{$portofolioname}{$ppos}
                              {Pos_Buy_Price}
                        )
                      )
                    {
                        $self->{Portofolios}{$portofolioname}{$ppos}
                          {Share_Buy_Price} =
                          $self->{Portofolios}{$portofolioname}{$ppos}
                          {Pos_Buy_Price} /
                          $self->{Portofolios}{$portofolioname}{$ppos}
                          {Pos_Quantity};
                    }
                    else {
                        $self->Trc(
                            'I', 2, 0x02201,
                            $self->{Portofolios}{$portofolioname}{$ppos}
                              {Pos_Quantity},
                            looks_like_number(
                                $self->{Portofolios}{$portofolioname}{$ppos}
                                  {Pos_Quantity}
                            ),
                            $self->{Portofolios}{$portofolioname}{$ppos}
                              {Pos_Buy_Price},
                            looks_like_number(
                                $self->{Portofolios}{$portofolioname}{$ppos}
                                  {Pos_Buy_Price}
                            )
                        );
                        $self->{Portofolios}{$portofolioname}{$ppos}
                          {Share_Buy_Price} = '';
                    }
                }
            }
        }
    }

    # Kurs ermitteln
    foreach ( sort keys %{ $self->{Kurs} } ) {
        $self->_webabruf( $_, $self->{Kurs}{$_}, keys %{ $self->{Flags} } );
    }
    $self->Trc( 'S', 1, 0x00002, $self->{subroutine} );
    $self->{subroutine} = $merker;

    return $rc;
}

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
    $self->{subroutine} = ( caller(0) )[3];
    $self->Trc( 'S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei} );

    no autovivification;

    my $rc = 0;

    my %summe;
    POSIX::setlocale( &POSIX::LC_ALL, 'de_DE' );
    foreach my $depot ( keys %{ $self->{Portofolios} } ) {
        foreach my $pos ( keys %{ $self->{Portofolios}{$depot} } ) {
            if ( my $symbol = ( split( ' ', $pos ) )[0] ) {
                if ( $self->{Kurs}{$symbol}{l1} ) {

                    # Werte ermitteln
                    $self->{Portofolios}{$depot}{$pos}{Pos_Price} =
                      $self->{Portofolios}{$depot}{$pos}{Pos_Quantity} *
                      $self->{Kurs}{$symbol}{l1} || 0;
                    $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change} =
                      $self->{Portofolios}{$depot}{$pos}{Pos_Quantity} *
                      $self->{Kurs}{$symbol}{c1} || 0;
                    $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change_Percent}
                      = $self->{Kurs}{$symbol}{p2};
                    $self->{Portofolios}{$depot}{$pos}{Pos_Change} =
                      $self->{Portofolios}{$depot}{$pos}{Pos_Price} -
                      $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price};
                    $self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent} =
                      100 * $self->{Portofolios}{$depot}{$pos}{Pos_Change} /
                      $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price};
                    $self->{Portofolios}{$depot}{$pos}{Pos_Div_Summe} =
                      $self->{Portofolios}{$depot}{$pos}{Pos_Quantity} *
                      $self->{Kurs}{$symbol}{d};

                    # Aufsummieren Portofolio und Gesamt
                    $summe{$depot}{Pos_Buy_Price} +=
                      $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price};
                    $summe{$depot}{Pos_Price} +=
                      $self->{Portofolios}{$depot}{$pos}{Pos_Price};
                    $summe{$depot}{Pos_Day_Change} +=
                      $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change};

        # Falls der Depotnamen eine Leerstelle enthaelt, wird das Depot nicht in
        # der Aufsummierung beruecksichtigt
                    ( my $depotname, my $sum ) = split( ' ', $depot );
                    if ( !defined($sum) ) {
                        $summe{Summe}{Pos_Buy_Price} +=
                          $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price};
                        $summe{Summe}{Pos_Price} +=
                          $self->{Portofolios}{$depot}{$pos}{Pos_Price};
                        $summe{Summe}{Pos_Day_Change} +=
                          $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change};
                    }
                }
            }
        }
        foreach my $pos ( keys %{ $self->{Portofolios}{$depot} } ) {
            if (   $summe{$depot}{Pos_Price}
                && $self->{Kurs}{ ( split( ' ', $pos ) )[0] }{l1} )
            {
                # Werte formatieren
                $self->{Portofolios}{$depot}{$pos}{Pos_Anteil} =
                  sprintf( '%5.2f',
                    100 * $self->{Portofolios}{$depot}{$pos}{Pos_Price} /
                      $summe{$depot}{Pos_Price} );
                $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price} =
                  sprintf( '%8.2f',
                    $self->{Portofolios}{$depot}{$pos}{Pos_Buy_Price} );
                $self->{Portofolios}{$depot}{$pos}{Pos_Price} =
                  sprintf( '%8.2f',
                    $self->{Portofolios}{$depot}{$pos}{Pos_Price} );
                $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change} =
                  sprintf( '%+8.2f',
                    $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change} );
                $self->{Portofolios}{$depot}{$pos}{Pos_Change} =
                  sprintf( '%+9.2f',
                    $self->{Portofolios}{$depot}{$pos}{Pos_Change} );
                $self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent} =
                  sprintf( '%+7.2f',
                    $self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent} );
                $self->{Portofolios}{$depot}{$pos}{Pos_Div_Summe} =
                  sprintf( '%7.2f',
                    $self->{Portofolios}{$depot}{$pos}{Pos_Div_Summe} );
                $self->{Portofolios}{$depot}{$pos}{Share_Buy_Price} =
                  sprintf( '%7.2f',
                    $self->{Portofolios}{$depot}{$pos}{Share_Buy_Price} );
            }
        }
    }

    foreach my $depot ( keys %summe ) {
        if ( $depot ne 'Summe' ) {
            ( my $depotname, my $sum ) = split( ' ', $depot );
            if ( defined($sum) || $depotname eq 'Summe' ) {
                $summe{$depot}{Pos_Anteil} = ' ';
            }
            else {
                $summe{$depot}{Pos_Anteil} = sprintf( '%5.2f',
                    100 * $summe{$depot}{Pos_Price} / $summe{Summe}{Pos_Price}
                );
            }

            # Portofoliosummen ermitteln und formatieren
            $summe{$depot}{Pos_Day_Change_Percent} = sprintf(
                '%+5.2f',
                100 * $summe{$depot}{Pos_Day_Change} / (
                    $summe{$depot}{Pos_Price} - $summe{$depot}{Pos_Day_Change}
                )
            );
            $summe{$depot}{Pos_Day_Change} =
              sprintf( '%+10.2f', $summe{$depot}{Pos_Day_Change} );
            $summe{$depot}{Pos_Change_Percent} = sprintf(
                '%+6.2f',
                100 * (
                    $summe{$depot}{Pos_Price} / $summe{$depot}{Pos_Buy_Price} -
                      1
                )
            );
            $summe{$depot}{Pos_Change} = sprintf( '%+11.2f',
                $summe{$depot}{Pos_Price} - $summe{$depot}{Pos_Buy_Price} );
            $summe{$depot}{Pos_Price} =
              sprintf( '%12.2f', $summe{$depot}{Pos_Price} );
            $summe{$depot}{Pos_Buy_Price} =
              sprintf( '%12.2f', $summe{$depot}{Pos_Buy_Price} );
        }
    }
    if ( defined( $summe{Summe} ) ) {
        $summe{Summe}{Pos_Anteil} = sprintf( '%5.2f', 100 );
        $summe{Summe}{Pos_Day_Change_Percent} = sprintf( '%+5.2f',
            100 * $summe{Summe}{Pos_Day_Change} /
              ( $summe{Summe}{Pos_Price} - $summe{Summe}{Pos_Day_Change} ) );
        $summe{Summe}{Pos_Day_Change} =
          sprintf( '%+10.2f', $summe{Summe}{Pos_Day_Change} );
        $summe{Summe}{Pos_Change_Percent} = sprintf( '%+6.2f',
            100 *
              ( $summe{Summe}{Pos_Price} / $summe{Summe}{Pos_Buy_Price} - 1 ) );
        $summe{Summe}{Pos_Change} = sprintf( '%+11.2f',
            $summe{Summe}{Pos_Price} - $summe{Summe}{Pos_Buy_Price} );
        $summe{Summe}{Pos_Price} =
          sprintf( '%12.2f', $summe{Summe}{Pos_Price} );
        $summe{Summe}{Pos_Buy_Price} =
          sprintf( '%12.2f', $summe{Summe}{Pos_Buy_Price} );
        $self->{Portofolios}{Summe} = \%summe;
    }

    foreach my $symbol ( keys %{ $self->{Kurs} } ) {
        foreach my $flag ( keys %{ $self->{Kurs}{$symbol} } ) {
            if ( $self->{Flags}{$flag}{Format} ) {
                $self->{Kurs}{$symbol}{$flag} = sprintf(
                    $self->{Flags}{$flag}{Format},
                    $self->{Kurs}{$symbol}{$flag}
                );
            }
        }
    }

    $self->Trc( 'S', 1, 0x00002, $self->{subroutine} );
    $self->{subroutine} = $merker;

    return $rc;
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
    $self->{subroutine} = ( caller(0) )[3];
    $self->Trc( 'S', 1, 0x00001, $self->{subroutine}, $self->{Eingabedatei} );

    no autovivification;

    my $rc = 0;

    my ( $dateiname, $format, $symbol );

    our $bg = 'on_black';

    sub colorize {

        # my $decider = (split(/,/, shift || 0))[0];
        ( my $decider = shift || 0 ) =~ s/^\s+|\s+$//g;
        $decider =~ s:,:\.:;
        my ( $m2, $m1, $p1, $p2 ) =
          ( shift || 0, shift || 0, shift || 0, shift || 0 );

        # my ($m2, $m1, $p1, $p2) = (-2, -1, 1, 2);
        $decider = 0 if ( !$decider );
        my $rc;
        if ( $m1 < $p1 ) {
            $rc = color( 'green ' . $bg ) if ( $decider >= $p2 );
            $rc = color( 'cyan ' . $bg )
              if ( $decider >= $p1 && $decider < $p2 );
            $rc = color( 'clear ' . $bg )
              if ( $decider > $m1 && $decider < $p1 );
            $rc = color( 'magenta ' . $bg )
              if ( $decider <= $m1 && $decider > $m2 );
            $rc = color( 'red ' . $bg ) if ( $decider <= $m2 );
        }
        elsif ( $m1 == $p1 ) {
            $rc = color( 'red ' . $bg ) if ( $decider >= $p2 );
            $rc = color( 'cyan ' . $bg )
              if ( $decider >= $p1 && $decider < $p2 );
            $rc = color( 'green ' . $bg ) if ( $decider < $p1 );
        }
        else {
            $rc = color( 'green ' . $bg ) if ( $decider <= $p2 );
            $rc = color( 'cyan ' . $bg )
              if ( $decider <= $p1 && $decider > $p2 );
            $rc = color( 'clear ' . $bg )
              if ( $decider < $m1 && $decider > $p1 );
            $rc = color( 'magenta ' . $bg )
              if ( $decider >= $m1 && $decider < $m2 );
            $rc = color( 'red ' . $bg ) if ( $decider >= $m2 );
        }

        return $rc;
    }

    my %ausgabedatei = $self->config('Ausgabedatei');

    foreach my $depot ( keys %{ $self->{Portofolios} } ) {
        $bg     = 'on_black';
        $format = "Name|$depot|";
        foreach my $pos ( keys %{ $self->{Portofolios}{Summe}{$depot} } ) {
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
        $format .= 'CK|'
          . colorize( $self->{Portofolios}{Summe}{$depot}{Pos_Change_Percent}
              || 0,
            -10, -5, 5, 10 )
          . '|';
        $format .= 'CT|'
          . colorize(
            $self->{Portofolios}{Summe}{$depot}{Pos_Day_Change_Percent} || 0,
            -2, -1, 1, 2 )
          . '|';
        $format .= 'CA|'
          . colorize( $self->{Portofolios}{Summe}{$depot}{Pos_Anteil} || 0,
            50, 40, 30, 20 )
          . '|';

        foreach my $ausgabe ( keys %ausgabedatei ) {
            if ( $self->{Portofolios}{Summe}{$depot} ) {
                if ( $depot eq 'Summe' ) {
                    if ( $self->config( 'Ausgabeformat', $ausgabe . '_SHead' ) )
                    {
                        push(
                            @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                            Utils::extendString(
                                $self->config(
                                    'Ausgabeformat', $ausgabe . '_SHead'
                                ),
                                $format
                              )
                              . "\n"
                        );
                        if (
                            my $line = $self->config(
                                'Ausgabeformat', $ausgabe . '_STrenn'
                            )
                          )
                        {
                            push(
                                @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                                $line . "\n"
                            );
                        }
                        push(
                            @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                            Utils::extendString(
                                $self->config(
                                    'Ausgabeformat', $ausgabe . '_SCol'
                                ),
                                $format
                              )
                              . "\n"
                        );
                        if (
                            my $line = $self->config(
                                'Ausgabeformat', $ausgabe . '_STrenn'
                            )
                          )
                        {
                            push(
                                @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                                $line . "\n"
                            );
                        }
                    }
                }
                else {
                    if ( $self->config( 'Ausgabeformat', $ausgabe . '_Head' ) )
                    {
                        push(
                            @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                            Utils::extendString(
                                $self->config(
                                    'Ausgabeformat', $ausgabe . '_Head'
                                ),
                                $format
                              )
                              . "\n"
                        );
                        if (
                            my $line = $self->config(
                                'Ausgabeformat', $ausgabe . '_Trenn'
                            )
                          )
                        {
                            push(
                                @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                                $line . "\n"
                            );
                        }
                        push(
                            @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                            Utils::extendString(
                                $self->config(
                                    'Ausgabeformat', $ausgabe . '_Col'
                                ),
                                $format
                              )
                              . "\n"
                        );
                        if (
                            my $line = $self->config(
                                'Ausgabeformat', $ausgabe . '_Trenn'
                            )
                          )
                        {
                            push(
                                @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                                $line . "\n"
                            );
                        }
                    }
                }
            }
        }

        # Ausgabe der Einzeltitel
        my %sortlist;
        if ( $depot eq 'Summe' ) {
            foreach my $pos ( keys %{ $self->{Portofolios}{$depot} } ) {
                if ( $pos ne 'Summe' && ( split( ' ', $pos ) )[0] ne '1' ) {
                    $sortlist{$pos} = $pos;
                }
            }
        }
        else {
            foreach my $pos ( keys %{ $self->{Portofolios}{$depot} } ) {
                $symbol = ( split( ' ', $pos ) )[0];
                if ( $self->{Kurs}{$symbol}{n} ) {
                    $sortlist{ $self->{Kurs}{$symbol}{n} . $pos } = $pos;
                }
            }
        }

        foreach ( sort( keys %sortlist ) ) {
            my $pos = $sortlist{$_};
            if ( $pos ne 'Summe' ) {
                $bg = $bg eq 'on_black' ? 'on_blue' : 'on_black';
                $symbol = ( split( ' ', $pos ) )[0];
                $format = "Name|$symbol|";

                # Durchfuehren der Einzelwertformatierung und
                # Einsammeln aller allgemeinen Einzelwertattribute
                foreach my $flag ( keys %{ $self->{Kurs}{$symbol} } ) {
                    $format .= "$flag|$self->{Kurs}{$symbol}{$flag}|";
                }

                # Einsammeln aller depotspezifischen Einzelwertattribute
                foreach ( keys %{ $self->{Portofolios}{$depot}{$pos} } ) {
                    $format .= "$_|$self->{Portofolios}{$depot}{$pos}{$_}|";
                }
                $format .= 'CC|' . color('clear') . '|';
                $format .= 'CK|'
                  . colorize(
                    $self->{Portofolios}{$depot}{$pos}{Pos_Change_Percent} || 0,
                    -10, -5, 5, 10
                  ) . '|';
                $format .= 'CT|'
                  . colorize(
                    $self->{Portofolios}{$depot}{$pos}{Pos_Day_Change_Percent}
                      || 0,
                    -2, -1, 1, 2
                  ) . '|';
                if ( $depot eq 'Summe' ) {
                    $format .= 'CA|'
                      . colorize( $self->{Portofolios}{$depot}{$pos}{Pos_Anteil}
                          || 0,
                        50, 40, 30, 20 )
                      . '|';
                }
                else {
                    $format .= 'CA|'
                      . colorize( $self->{Portofolios}{$depot}{$pos}{Pos_Anteil}
                          || 0,
                        5, 4, 3, 2 )
                      . '|';
                    $format .= 'CD|'
                      . colorize( $self->{Kurs}{$symbol}{y} || 0, 1, 2, 3, 4 )
                      . '|';
                    $format .= 'CL|'
                      . colorize( $self->{Kurs}{$symbol}{_lastTrade}, 3, 2, 1,
                        0 )
                      . '|';
                }
            }
            foreach my $ausgabe ( keys %ausgabedatei ) {
                if ( $depot eq 'Summe' ) {
                    ( my $depotname, my $sum ) = split( ' ', $depot );
                    if ( !defined($sum) ) {
                        if (
                            $self->config(
                                'Ausgabeformat', $ausgabe . '_SBody'
                            )
                          )
                        {
                            push(
                                @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                                Utils::extendString(
                                    $self->config(
                                        'Ausgabeformat', $ausgabe . '_SBody'
                                    ),
                                    $format
                                  )
                                  . "\n"
                            );
                        }
                    }
                }
                else {
                    if ( $self->config( 'Ausgabeformat', $ausgabe . '_Body' ) )
                    {
                        push(
                            @{ $self->{Ausgabe}{$ausgabe}{$depot} },
                            Utils::extendString(
                                $self->config(
                                    'Ausgabeformat', $ausgabe . '_Body'
                                ),
                                $format
                              )
                              . "\n"
                        );
                    }
                }
            }
        }
    }

    # Ausgabedateien schreiben
    foreach my $ausgabe ( keys %{ $self->{Ausgabe} } ) {
        foreach my $depot ( keys %{ $self->{Ausgabe}{$ausgabe} } ) {
            if ( !$self->argument(0) || $depot ne 'Summe' ) {

# Anlegen
#$self->Log($ausgabe, Utils::extendString($ausgabedatei{$ausgabe} . '.tmp', "Depot|$depot")) or $self->Exit(1, 0, 0x00010, $ausgabedatei{$ausgabe}, "Depot|$depot");
                my $dateiname =
                  Utils::extendString( $ausgabedatei{$ausgabe},
                    "Depot|$depot" );
                open( DEPOT, '> ' . $dateiname . '.tmp' );
                while ( my $line =
                    shift( @{ $self->{Ausgabe}{$ausgabe}{$depot} } ) )
                {
                    print DEPOT $line;
                }

                # Schliessen und Umbenennen
                #$self->TerminateLog($ausgabe);
                close(DEPOT);
                move( $dateiname . '.tmp', $dateiname );
            }
        }
    }

    # Kursinfos löschen
    if ( exists( $self->{VarFile} ) ) {
        eval { unlink( $self->{VarFile} ); };
    }

    $self->Trc( 'S', 1, 0x00002, $self->{subroutine} );
    $self->{subroutine} = $merker;

    return $rc;
}

1;
