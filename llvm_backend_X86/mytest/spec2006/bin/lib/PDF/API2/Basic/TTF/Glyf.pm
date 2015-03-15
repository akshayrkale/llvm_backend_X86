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
#   $Id: Glyf.pm,v 1.5 2004/06/15 09:13:37 fredo Exp $
#
#=======================================================================
package PDF::API2::Basic::TTF::Glyf;

=head1 NAME

PDF::API2::Basic::TTF::Glyf - The Glyf data table

=head1 DESCRIPTION

This is a stub table. The real data is held in the loca table. If you want to get a glyf
look it up in the loca table as C<$f->{'loca'}{'glyphs'}[$num]>. It won't be here!

The difference between reading this table as opposed to the loca table is that
reading this table will cause updated glyphs to be written out rather than just
copying the glyph information from the input file. This causes font writing to be
slower. So read the glyf as opposed to the loca table if you want to change glyf
data. Read the loca table only if you are just wanting to read the glyf information.

This class is used when writing the glyphs though.

=head1 METHODS

=cut


use strict;
use vars qw(@ISA);
@ISA = qw(PDF::API2::Basic::TTF::Table);

=head2 $t->read

Reads the C<loca> table instead!

=cut

sub read
{
    my ($self) = @_;

    $self->{' PARENT'}{'loca'}->read;
    $self->{' read'} = 1;
    $self;
}


=head2 $t->out($fh)

Writes out all the glyphs in the parent's location table, calculating a new
output location for each one.

=cut

sub out
{
    my ($self, $fh) = @_;
    my ($i, $loca, $offset, $numGlyphs);

    return $self->SUPER::out($fh) unless $self->{' read'};

    $loca = $self->{' PARENT'}{'loca'}{'glyphs'};
    $numGlyphs = $self->{' PARENT'}{'maxp'}{'numGlyphs'};

    $offset = 0;
    for ($i = 0; $i < $numGlyphs; $i++)
    {
        next unless defined $loca->[$i];
        $loca->[$i]->update;
        $loca->[$i]{' OUTLOC'} = $offset;
        $loca->[$i]->out($fh);
        $offset += $loca->[$i]{' OUTLEN'};
    }
    $self->{' PARENT'}{'head'}{'indexToLocFormat'} = ($offset >= 0x20000);
    $self;
}


=head2 $t->out_xml($context, $depth)

Outputs all the glyphs in the glyph table just where they are supposed to be output!

=cut

sub out_xml
{
    my ($self, $context, $depth) = @_;
    my ($fh) = $context->{'fh'};
    my ($loca, $i, $numGlyphs);

    $loca = $self->{' PARENT'}{'loca'}{'glyphs'};
    $numGlyphs = $self->{' PARENT'}{'maxp'}{'numGlyphs'};

    for ($i = 0; $i < $numGlyphs; $i++)
    {
        $context->{'gid'} = $i;
        $loca->[$i]->out_xml($context, $depth) if (defined $loca->[$i]);
    }

    $self;
}


=head2 $t->XML_start($context, $tag, %attrs)

Pass control to glyphs as they occur

=cut

sub XML_start
{
    my ($self) = shift;
    my ($context, $tag, %attrs) = @_;

    if ($tag eq 'glyph')
    {
        $context->{'tree'}[-1] = PDF::API2::Basic::TTF::Glyph->new(read => 2, PARENT => $self->{' PARENT'});
        $context->{'receiver'} = $context->{'tree'}[-1];
    }
}


=head2 $t->XML_end($context, $tag, %attrs)

Collect up glyphs and put them into the loca table

=cut

sub XML_end
{
    my ($self) = shift;
    my ($context, $tag, %attrs) = @_;

    if ($tag eq 'glyph')
    {
        unless (defined $context->{'glyphs'})
        {
            if (defined $self->{' PARENT'}{'loca'})
            { $context->{'glyphs'} = $self->{' PARENT'}{'loca'}{'glyphs'}; }
            else
            { $context->{'glyphs'} = []; }
        }
        $context->{'glyphs'}[$attrs{'gid'}] = $context->{'tree'}[-1];
        return $context;
    } else
    { return $self->SUPER::XML_end(@_); }
}

1;

=head1 BUGS

None known

=head1 AUTHOR

Martin Hosken Martin_Hosken@sil.org. See L<PDF::API2::Basic::TTF::Font> for copyright and
licensing.

=cut

