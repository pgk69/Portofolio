eval 'exec perl -wS $0 ${1+"$@"}'
  if 0;

#-------------------------------------------------------------------------------
# Letzte Aenderung:     $Date: 2012-03-05 10:49:17 +0100 (Mo, 05 Mrz 2012) $
#                       $Revision:  $
#                       $Author:  $
#
# Aufgabe:			- was tun
#
# $Id: portofolio.pl 785 2012-03-05 09:49:17Z $
# $URL: $
#-------------------------------------------------------------------------------

# Letzte Aenderung:

use warnings;    # on by default

#use utf8;      # so literals and identifiers can be in UTF-8
#use v5.12;     # or later to get "unicode_strings" feature
#use strict;    # quote strings, declare variables
#use warnings  qw(FATAL utf8);    # fatalize encoding glitches
#use open      qw(:std :utf8);    # undeclared streams in UTF-8
#use charnames qw(:full :short);  # unneeded in v5.16
#binmode STDOUT, ":utf8";

# use 5.010;
use vars qw($VERSION $SVN);

use constant SVN_ID => '($Id: programm.pl 785 2012-03-05 09:49:17Z xck10e7 $)

$Author: xck10e7 $ 

$Revision: 785 $ 
$Date: 2012-03-05 10:49:17 +0100 (Mo, 05 Mrz 2012) $ 

$URL: https://svn.fiducia.de/svn/multicom/trunk/multicom/Framework%20OO/programm.pl $

';

# Extraktion der Versionsinfo aus der SVN Revision
( $VERSION = SVN_ID ) =~ s/^(.*\$Revision: )([0-9]*)(.*)$/1.0 R$2/ms;
$SVN = $VERSION . ' ' . SVN_ID;

$| = 1;

# use lib $Bin . "/lib";       # fuer Aufruf mit voll qualifiziertem Pfad noetig
use lib "./lib";
use lib "/Users/pgk/Documents/00_Eclipse/Framework/lib";

#
# Module
#
use CmdLine;
use Trace;
use Configuration;
use DBAccess;

use PORTOFOLIO;

# use PORTOFOLIO::Modul1;
# use PORTOFOLIO::Modul2;

use Fcntl;
use FindBin qw($Bin $Script $RealBin $RealScript);
use Data::Dumper;

#
# Variablendefinition
#

#
# Objektdefinition
#

# Option-Objekt: Liest und speichert die Kommandozeilenparameter
my $cmdLine = CmdLine->new( 'flush' => 'flush' );
$cmdLine->version($VERSION);

# Trace-Objekt: Liest und speichert die Meldungstexte; gibt Tracemeldungen aus
my $trace = Trace->new();
$trace->version($VERSION);

# Config-Objekt: Liest und speichert die Initialisierungsdatei
my $config = Configuration->new();
$config->version($VERSION);

## Datenbank-Objekt: Regelt dei Datenbankzugriffe
#my $dbaccess = DBAccess->new();
#$dbaccess->version($VERSION);

# Kopie des Fehlerkanals erstellen zur gelegentlichen Abschaltung
no warnings;
sysopen( MYERR, "&STDERR", O_WRONLY );
use warnings;

#
################################################################################
## main
################################################################################
#
$trace->Trc( 'S', 1, 0x00001, $config->prg,
      $VERSION . " ("
    . $$ . ") "
    . " Test: "
    . $trace->test
    . "  Parameter: "
    . $cmdLine->{ArgStrg} );

# Test der Kommandozeilenparameter
if ( $cmdLine->option('Help') || $cmdLine->option('Version') ) {
  $cmdLine->usage;
  if ( $cmdLine->option('Help') || $cmdLine->option('Version') ) {
    $trace->Exit( 0, 1, 0x00002, $config->prg, $VERSION );
  }
  $trace->Exit( 1, 0, 0x0f003, $cmdLine->{ArgStrg} );
}

my $prg;
eval { $prg = PORTOFOLIO->new() };

if ($@) {
  $prg->Exit( 0, 1, 0x0ffff, $prg->prg, $VERSION );
}
$prg->version($VERSION);

#-------------------------------------------------------------------------------
# PRGRAMM-Start
#-------------------------------------------------------------------------------
$prg->lese_Portofolios( $cmdLine->argument() );
$prg->analysiere_Portofolios();
$prg->schreibe_Ausgabe();

$prg->Exit( 0, 1, 0x00002, $prg->prg, $VERSION );

exit 1;
