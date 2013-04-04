package Pod::Weaver::Plugin::Perinci;

use 5.010001;
use Moose;
with 'Pod::Weaver::Role::Section';

use Perinci::To::POD;
use Pod::Elemental;
use Pod::Elemental::Element::Nested;

# VERSION

# regex
has exclude_modules => (
    is => 'rw',
    isa => 'Str',
);
has exclude_files => (
    is => 'rw',
    isa => 'Str',
);

sub weave_section {
    my ($self, $document, $input) = @_;

    my $filename = $input->{filename} || 'file';

    # guess package name from filename
    my $package;
    if ($filename =~ m!^lib/(.+)\.pm$!) {
        $package = $1;
        $package =~ s!/!::!g;
    } else {
        $self->log_debug(["skipped file %s (not a Perl module)", $filename]);
        return;
    }

    if (defined $self->exclude_files) {
        my $re = $self->exclude_files;
        eval { $re = qr/$re/ };
        $@ and die "Invalid regex in exclude_files: $re";
        if ($filename =~ $re) {
            $self->log_debug(["skipped file %s (matched exclude_files)", $filename]);
            return;
        }
    }
    if (defined $self->exclude_modules) {
        my $re = $self->exclude_modules;
        eval { $re = qr/$re/ };
        $@ and die "Invalid regex in exclude_modules: $re";
        if ($package =~ $re) {
            $self->log (["skipped package %s (matched exclude_modules)", $package]);
            return;
        }
    }

    local @INC = ("lib", @INC);

    $self->log_debug(["generating POD for %s ...", $filename]);

    # generate the POD and insert it to FUNCTIONS section
    my $url = $package; $url =~ s!::!/!g; $url .= "/";
    my $doc = Perinci::To::POD->new(url => $url);
    $doc->delete_doc_section('summary'); # already handled by other plugins
    $doc->delete_doc_section('version'); # ditto
    my $pod_text = $doc->generate_doc;

    my $found;
    while ($pod_text =~ /^=head1 ([^\n]+)\n(.+?)(?=^=head1|\z)/msg) {
        $found++;
        my $fpara = Pod::Elemental::Element::Nested->new({
            command  => 'head1',
            content  => $1,
            children => Pod::Elemental->read_string($2)->children,
        });
        push @{ $input->{pod_document}->children }, $fpara;
    }
    if ($found) {
        $self->log(["added POD sections from Rinci metadata for %s", $filename]);
    }
}

1;
# ABSTRACT: Insert POD from Rinci metadata

=for Pod::Coverage weave_section

=head1 SYNOPSIS

In your C<weaver.ini>:

 [-Perinci]
 ;exclude_modules = REGEX
 ;exclude_files = REGEX


=head1 DESCRIPTION

This plugin inserts POD documentation (generated by L<Perinci::To::POD>).


=head1 TODO

If document already has =head1 FUNCTIONS, replace it instead of adding another
one. Same goes for =head1 DESCRIPTION (and later =head1 ATTRIBUTES, =head1
METHODS, =head1 VARIABLES).


=head1 SEE ALSO

L<Perinci::To::POD>

L<Pod::Weaver>
