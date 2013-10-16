package Modul1;

#-------------------------------------------------------------------------------------------------
# Letzte Aenderung:     $Date: 2012-03-05 10:49:17 +0100 (Mo, 05 Mrz 2012) $
#						$Revision: 785 $
#                       $Author: xck10e7 $
#
# Aufgabe:				- Ausfuehrbarer Code von MT940.pm
#
# $Id: Modul1.pm 785 2012-03-05 09:49:17Z xck10e7 $
# $URL: https://svn.fiducia.de/svn/multicom/trunk/multicom/Framework%20OO/lib/PROGRAMM/Modul1.pm $
#-------------------------------------------------------------------------------------------------

use 5.004;
use strict;

use base 'Exporter';

our @EXPORT    = ();
our @EXPORT_OK = ();

use vars @EXPORT, @EXPORT_OK;

use vars qw( @ISA );
@ISA = qw(SPLIT);

#
# Module
#

#
# Konstantendefinition
#

#
# Variablendefinition
#

#
# Methodendefinition
#

sub mache_was {
  #################################################################
  #     Was machen
  my $self = shift;
  $self->{subroutine} = (caller(0))[3];

  $self->Trc( 'S', 1, 0x00001, $self->{subroutine} );
  my $rc = 1;

  $self->Trc( 'S', 1, 0x00002, $self->{subroutine} );
  $self->{subroutine} = '';
  # Explizite Uebergabe des Returncodes noetig, da sonst ein Fehler auftritt
  return $rc;
}
1;
