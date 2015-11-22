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

use utf8;      # so literals and identifiers can be in UTF-8
use v5.12;     # or later to get "unicode_strings" feature
use strict;    # quote strings, declare variables
use warnings  qw(FATAL utf8);    # fatalize encoding glitches
 
use open      qw(:std :utf8);    # undeclared streams in UTF-8
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
($VERSION = SVN_ID) =~ s/^(.*\$Revision: )([0-9]*)(.*)$/1.0 R$2/ms;
$SVN = $VERSION . ' ' . SVN_ID;

$| = 1;

use FindBin qw($Bin $Script $RealBin $RealScript);
# use lib $Bin . "/lib";       # fuer Aufruf mit voll qualifiziertem Pfad noetig
use lib "${RealBin}/lib";
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
# use Data::Dumper;

#
# Variablendefinition
#

#
# Objektdefinition
#

# Option-Objekt: Liest und speichert die Kommandozeilenparameter
$VERSION = CmdLine->new()->version($VERSION);

# Trace-Objekt: Liest und speichert die Meldungstexte; gibt Tracemeldungen aus
$VERSION = Trace->new()->version($VERSION);

# Config-Objekt: Liest und speichert die Initialisierungsdatei
$VERSION = Configuration->new()->version($VERSION);

# Datenbank-Objekt: Regelt die Datenbankzugriffe
$VERSION = DBAccess->new()->version($VERSION);

# Kopie des Fehlerkanals erstellen zur gelegentlichen Abschaltung
no warnings;
sysopen(MYERR, "&STDERR", O_WRONLY);
use warnings;

#
#################################################################
## main
##################################################################
#

my $prg;
eval {$prg = PORTOFOLIO->new()};

if ($@) {
  Trace->Exit(0, 1, 0x0ffff, Configuration->prg, $VERSION);
}

#-------------------------------------------------------------------------------
# PRGRAMM-Start
# 1.) Beim Parsen der Portofolios BKN.DE|BNS|BNS.TO splitten nach "|", sortier-
#     en vom ersten Wert Alles vor dem Komma als Symbol verwenden.
#     Der Key ist "BKN 1".
#     Im Inhalt ist ein Feld Symbol_Local mit "BKN.DE|BNS|BNS.TO"
# 2.) Für alle gefundenen lokalen Symbole einen Kurseintrag anlegen mit dem
#     lokalen Symbol als key.
#       {BKN.DE}
#       {BNS.TO}
#       {BNS}
# 3.) Für alle Kurse die Werte holen
# 4.) Für alle Portofoliopositionen den enstprechenden Kurs Eintrag suchen
#       - Key des Kurses ist ein Symbol_lokal der Portofolioposition
#       - Währung is identisch
# 5.) Falls es Im Kurs Dividende und Dividenden Währung gibt, übernehmen
#     und Werte aus Portofolio überschreiben
# 6.) Alle Positionen umrechnen in die Basiswährung
# 7.) Ggf. aufsummieren und ausgeben
#        
#     
#-------------------------------------------------------------------------------
$prg->Flags_laden();
$prg->Portofolios_lesen(CmdLine->argument());

$prg->Cash_extrahieren();
$prg->Gesamtliste_erzeugen() if ($prg->{Gesamtliste});
$prg->Positionen_parsen();

$prg->Wechselkurse_lesen();
$prg->Kurse_ermitteln();

$prg->Kurse_umrechnen();
$prg->Portofolios_summieren() if ($prg->{SumPos});
$prg->Portofolios_analysieren();
$prg->Ausgabe_schreiben();

$prg->Exit(0, 1, 0x00002, $prg->prg, $VERSION);

exit 1;
