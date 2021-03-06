#=======================================================================
#    ____  ____  _____              _    ____ ___   ____
#   |  _ \|  _ \|  ___|  _   _     / \  |  _ \_ _| |___ \
#   | |_) | | | | |_    (_) (_)   / _ \ | |_) | |    __) |
#   |  __/| |_| |  _|    _   _   / ___ \|  __/| |   / __/
#   |_|   |____/|_|     (_) (_) /_/   \_\_|  |___| |_____|
#
#   A Perl Module Chain to faciliate the Creation and Modification
#   of High-Quality "Portable Document Format (PDF)" Files.
#
#=======================================================================
#
#   THIS IS A REUSED PERL MODULE, FOR PROPER LICENCING TERMS SEE BELOW:
#
#
#   Copyright Martin Hosken <Martin_Hosken@sil.org>
#
#   No warranty or expression of effectiveness, least of all regarding
#   anyone's safety, is implied in this software or documentation.
#
#   This specific module is licensed under the Perl Artistic License.
#
#
#   $Id: Hhea.pm,v 1.6 2005/02/21 22:15:45 fredo Exp $
#
#=======================================================================
package PDF::API2::Basic::TTF::Hhea;

=head1 NAME

PDF::API2::Basic::TTF::Hhea - Horizontal Header table

=head1 DESCRIPTION

This is a simplte table with just standards specified instance variables

=head1 INSTANCE VARIABLES

    version
    Ascender
    Descender
    LineGap
    advanceWidthMax
    minLeftSideBearing
    minRightSideBearing
    xMaxExtent
    caretSlopeRise
    caretSlopeRun
    metricDataFormat
    numberOfHMetrics


=head1 METHODS

=cut

use strict;
use vars qw(@ISA %fields @field_info);

require PDF::API2::Basic::TTF::Table;
use PDF::API2::Basic::TTF::Utils;

@ISA = qw(PDF::API2::Basic::TTF::Table);
@field_info = (
    'version' => 'f',
    'Ascender' => 's',
    'Descender' => 's',
    'LineGap' => 's',
    'advanceWidthMax' => 'S',
    'minLeftSideBearing' => 's',
    'minRightSideBearing' => 's',
    'xMaxExtent' => 's',
    'caretSlopeRise' => 's',
    'caretSlopeRun' => 's',
    'metricDataFormat' => '+10s',
    'numberOfHMetrics' => 'S');

sub init
{
    my ($k, $v, $c, $i);
    for ($i = 0; $i < $#field_info; $i += 2)
    {
        ($k, $v, $c) = TTF_Init_Fields($field_info[$i], $c, $field_info[$i + 1]);
        next unless defined $k && $k ne "";
        $fields{$k} = $v;
    }
}


=head2 $t->read

Reads the table into memory as instance variables

=cut

sub read
{
    my ($self) = @_;
    my ($dat);

    $self->SUPER::read or return $self;
    init unless defined $fields{'Ascender'};
    $self->{' INFILE'}->read($dat, 36);

    TTF_Read_Fields($self, $dat, \%fields);
    $self;
}


=head2 $t->out($fh)

Writes the table to a file either from memory or by copying.

=cut

sub out
{
    my ($self, $fh) = @_;

    return $self->SUPER::out($fh) unless $self->{' read'};

    $self->{'numberOfHMetrics'} = $self->{' PARENT'}{'hmtx'}->numMetrics || $self->{'numberOfHMetrics'};
    $fh->print(TTF_Out_Fields($self, \%fields, 36));
    $self;
}


=head2 $t->update

Updates various parameters in the hhea table from the hmtx table, assuming
the C<hmtx> table is dirty.

=cut

sub update
{
    my ($self) = @_;
    my ($hmtx) = $self->{' PARENT'}{'hmtx'};
    my ($glyphs);
    my ($num);
    my ($i, $maw, $mlsb, $mrsb, $mext, $aw, $lsb, $ext);

    return undef unless ($self->SUPER::update);
    return undef unless (defined $hmtx && defined $self->{' PARENT'}{'loca'});

    $hmtx->read->update;
    $self->{' PARENT'}{'loca'}->read->update;
    $glyphs = $self->{' PARENT'}{'loca'}{'glyphs'};
    $num = $self->{' PARENT'}{'maxp'}{'numGlyphs'};

    return undef unless ($hmtx->{' isDirty'} || $self->{' PARENT'}{'loca'}{' isDirty'});

    for ($i = 0; $i < $num; $i++)
    {
        $aw = $hmtx->{'advance'}[$i];
        $lsb = $hmtx->{'lsb'}[$i];
        if (defined $glyphs->[$i])
        { $ext = $lsb + $glyphs->[$i]->read->{'xMax'} - $glyphs->[$i]{'xMin'}; }
        else
        { $ext = $aw; }
        $maw = $aw if ($aw > $maw);
        $mlsb = $lsb if ($lsb < $mlsb or $i == 0);
        $mrsb = $aw - $ext if ($aw - $ext < $mrsb or $i == 0);
        $mext = $ext if ($ext > $mext);
    }
    $self->{'advanceWidthMax'} = $maw;
    $self->{'minLeftSideBearing'} = $mlsb;
    $self->{'minRightSideBearing'} = $mrsb;
    $self->{'xMaxExtent'} = $mext;
    $self->{'numberOfHMetrics'} = $hmtx->numMetrics;
    $self;
}


1;


=head1 BUGS

None known

=head1 AUTHOR

Martin Hosken Martin_Hosken@sil.org. See L<PDF::API2::Basic::TTF::Font> for copyright and
licensing.

=cut

