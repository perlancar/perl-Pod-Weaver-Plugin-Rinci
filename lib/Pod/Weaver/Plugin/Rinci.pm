package Pod::Weaver::Plugin::Rinci;

# DATE
# VERSION

use 5.010001;
use Moose;
with 'Pod::Weaver::Role::Section';

use List::Util qw(first);
use Perinci::Access::Perl;
use Perinci::To::POD;
use Pod::Elemental;
use Pod::Elemental::Element::Nested;

our $pa = Perinci::Access::Perl->new(
    # we want to document the function's original properties (i.e. result_naked
    # and args_as)
    normalize_metadata => 0,
);

# regex
has exclude_modules => (
    is => 'rw',
    isa => 'Str',
);
has exclude_files => (
    is => 'rw',
    isa => 'Str',
);

sub _process_module {
    my ($self, $document, $input) = @_;

    my $filename = $input->{filename};

    # guess package from filename
    $filename =~ m!^lib/(.+)\.pm$!;
    my $package = $1;
    $package =~ s!/!::!g;

    # XXX handle dynamically generated module (if there is such thing in the
    # future)
    local @INC = ("lib", @INC);

    my $url = $package; $url =~ s!::!/!g; $url = "pl:/$url/";
    my $res = $pa->request(meta => $url);
    die "Can't meta $url: $res->[0] - $res->[1]" unless $res->[0] == 200;
    my $meta = $res->[2];
    $res = $pa->request(child_metas => $url);
    die "Can't child_metas $url: $res->[0] - $res->[1]" unless $res->[0] == 200;
    my $cmetas = $res->[2];

    my $doc = Perinci::To::POD->new(
        name=>$package, meta=>$meta, child_metas=>$cmetas);
    $doc->delete_doc_section('summary'); # already handled by other plugins
    $doc->delete_doc_section('version'); # ditto
    my $pod_text = $doc->gen_doc;

    my $found;
    while ($pod_text =~ /^=head1 ([^\n]+)\n(.+?)(?=^=head1|\z)/msg) {
        my ($sectname, $sectcontent) = ($1, $2);

        # skip inserting section if there is no text
        next unless $sectcontent =~ /\S/;

        # skip inserting FUNCTIONS if there are no functions
        next if $sectname =~ /functions/i && $sectcontent !~ /^=head2/m;

        $found++;
        #$self->log(["generated POD section %s", $1]);
        my $elem = Pod::Elemental::Element::Nested->new({
            command  => 'head1',
            content  => $sectname,
            children => Pod::Elemental->read_string($sectcontent)->children,
        });
        my $sect = first {
            $_->can('command') && $_->command eq 'head1' &&
                uc($_->{content}) eq uc($sectname) }
            @{ $document->children }, @{ $input->{pod_document}->children };
        # if existing section exists, append it
        #$self->log(["sect=%s", $sect]);
        if ($sect) {
            # sometimes we get a Pod::Elemental::Element::Pod5::Command (e.g.
            # empty "=head1 DESCRIPTION") instead of a
            # Pod::Elemental::Element::Nested. in that case, just ignore it.
            if ($sect->can('children')) {
                push @{ $sect->children }, @{ $elem->children };
            }
        } else {
            push @{ $document->children }, $elem;
        }
    }
    if ($found) {
        $self->log(["added POD sections from Rinci metadata for %s", $filename]);
    }
}

sub _process_script {
    my ($self, $document, $input) = @_;

    my $filename = $input->{filename};

    # find file object
    my $file;
    for (@{ $input->{zilla}->files }) {
        if ($_->name eq $filename) {
            $file = $_;
            last;
        }
    }
    die "Can't find file object for $filename" unless $file;

    # check if script really uses Perinci::CmdLine
    my $ct = $file->content;
    unless ($ct =~ /\b(?:use|require)\s+
                    (Perinci::CmdLine(?:::Lite|::Any)?)\b/x) {
        $self->log_debug(["skipped script %s (doesn't seem to use Perinci::CmdLine)", $filename]);
        return;
    }

    require UUID::Random;
    my $tag=UUID::Random::generate();

    my @cmd = ($^X, "-MPerinci::CmdLine::Base::Patch::DumpOnRun=-tag,$tag");
    if ($file->isa("Dist::Zilla::File::OnDisk")) {
        push @cmd, $filename;
    } else {
        # write content to filesystem to temp file first
        my ($fh, $tempname) = tempfile();
        print $fh $file->content;
        close $fh;
        push @cmd, $tempname;
    }
    push @cmd, "--version";

    say "D:", join(" ",@cmd);
    $self->log_debug(["running script with %s (doesn't seem to use Perinci::CmdLine)", $filename]);
    system @cmd;
}

sub weave_section {
    my ($self, $document, $input) = @_;

    my $filename = $input->{filename};

    if (defined $self->exclude_files) {
        my $re = $self->exclude_files;
        eval { $re = qr/$re/ };
        $@ and die "Invalid regex in exclude_files: $re";
        if ($filename =~ $re) {
            $self->log_debug(["skipped file %s (matched exclude_files)", $filename]);
            return;
        }
    }

    my $package;
    if ($filename =~ m!^lib/(.+)\.pm$!) {
        $package = $1;
        $package =~ s!/!::!g;
        if (defined $self->exclude_modules) {
            my $re = $self->exclude_modules;
            eval { $re = qr/$re/ };
            $@ and die "Invalid regex in exclude_modules: $re";
            if ($package =~ $re) {
                $self->log (["skipped package %s (matched exclude_modules)", $package]);
                return;
            }
        }
        $self->_process_module($document, $input);
    } elsif ($filename =~ m!^(?:bin|scripts?)/!) {
        $self->_process_script($document, $input);
    }
}

1;
# ABSTRACT: Insert stuffs to POD from Rinci metadata

=for Pod::Coverage weave_section

=head1 SYNOPSIS

In your C<weaver.ini>:

 [-Rinci]
 ;exclude_modules = REGEX
 ;exclude_files = REGEX


=head1 DESCRIPTION

This plugin inserts stuffs to POD documentation based on information found on
Rinci metadata.

For modules, the following are inserted:

=over

=item * DESCRIPTION

From C<description> property from package metadata, if any.

=item * FUNCTIONS

Documentation for each function for which the metadata is found under the
package will be added here. For each function, there will be summary,
description, usage, list of arguments and their documentation, as well as
examples, according to what's available in the function metadata of
corresponding function.

=back

To get Rinci metadata from a module, L<Perinci::Access::Perl> is used.

For scripts using L<Perinci::CmdLine>, the following are inserted:

=over

=item * DESCRIPTION

If the script does not have subcommands, description from function metadata will
be inserted here, if any.

=item * SUBCOMMANDS

If the script has subcommands, each subcommand will be listed here, along with
its summary and description.

=item * OPTIONS

Command-line options for the script will be listed here. If script has
subcommands, the options will be categorized per subcommand.

=item * FILES

Configuration files read by script will be listed here.

=back

To get Perinci::CmdLine information, the script is run with a patched C<run()>
that will dump the content of the object and exit immediately, so the plugin can
inspect it.


=head1 SEE ALSO

L<Pod::Weaver>
