package Pod::Weaver::Plugin::Rinci;

# DATE
# VERSION

use 5.010001;
use Moose;
with 'Pod::Weaver::Role::AddTextToSection';
with 'Pod::Weaver::Role::Section';
with 'Pod::Weaver::Role::DumpPerinciCmdLineScript';

use Data::Dmp qw(dmp);
use Encode qw(decode encode);
use File::Temp qw(tempfile);
use IPC::System::Options;
use Markdown::To::POD;
use Perinci::Access::Perl;
use Perinci::Sub::To::CLIDocData qw(gen_cli_doc_data_from_meta);
use Perinci::To::POD;
use PERLANCAR::ShellQuote::Any;
use Proc::ChildError qw(explain_child_error);
use Scalar::Util qw(blessed);
use Sub::Identify qw(sub_fullname);

our $pa = Perinci::Access::Perl->new;

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
    my ($file) = grep { $_->name eq $filename } @{ $input->{zilla}->files };

    unless ($file->isa("Dist::Zilla::File::OnDisk")) {
        $self->log_debug(["skipping %s: not an ondisk file, currently only ondisk files are processed", $filename]);
        return;
    }

    # guess package from filename
    $filename =~ m!^lib/(.+)\.pm$!;
    my $package = $1;
    $package =~ s!/!::!g;

    local @INC = ("lib", @INC);

    my $url = $package; $url =~ s!::!/!g; $url = "pl:/$url/";
    my $res = $pa->request(meta => $url);
    die "Can't meta $url: $res->[0] - $res->[1]" unless $res->[0] == 200;
    my $meta = $res->[2];
    $res = $pa->request(child_metas => $url);
    die "Can't child_metas $url: $res->[0] - $res->[1]" unless $res->[0] == 200;
    my $cmetas = $res->[2];

    my $exports = {};
    {
        no strict 'refs';

        # we import specifically when module is using Exporter::Rinci as its
        # exporter, because Exporter::Rinci works by filling @EXPORT* variables
        # during import().
        if (sub_fullname($package->can("import")) =~ /^Exporter::Rinci::/) {
            $package->import;
        }

        my $uses_exporter_mod = @{"$package\::EXPORT"} || @{"$package\::EXPORT_OK"};
        for my $funcname (keys %$cmetas) {
            next unless $funcname =~ /\A\w+\z/;
            my $funcmeta = $cmetas->{$funcname};
            my $export = -1;
            if ($uses_exporter_mod &&
                    grep {$_ eq $funcname} @{"$package\::EXPORT"}) {
                $export = 1;
            } elsif ($uses_exporter_mod &&
                    grep {$_ eq $funcname} @{"$package\::EXPORT_OK"}) {
                $export = 0;
            } elsif ($uses_exporter_mod) {
                $export = -1;
            } elsif (grep {$_ eq 'export:default'} @{ $funcmeta->{tags} // [] }) {
                $export = 1;
            } elsif (grep {$_ eq 'export:never'} @{ $funcmeta->{tags} // [] }) {
                $export = -1;
            }
            $exports->{$funcname} = $export;
        }
    }
    my $doc = Perinci::To::POD->new(
        name=>$package, meta=>$meta, child_metas=>$cmetas, url=>$url,
        exports=>$exports);
    $doc->delete_doc_section('summary'); # already handled by other plugins
    $doc->delete_doc_section('version'); # ditto
    my $pod_text = $doc->gen_doc;

    my $found;
    while ($pod_text =~ /^=head1 ([^\n]+)\n(.+?)(?=^=head1|\z)/msg) {
        my ($sectname, $sectcontent) = ($1, $2);

        # skip inserting section if there is no text
        next unless $sectcontent =~ /\S/;

        # skip inserting FUNCTIONS if there are no functions
        next if $sectname eq 'FUNCTIONS' && $sectcontent !~ /^=head2/m;

        $found++;
        #$self->log(["generated POD section %s", $1]);

        my %opts;
        # position
        if ($sectname eq 'FUNCTIONS') {
            $opts{after_section} = [
                'DESCRIPTION',
            ];
            # make sure we don't put it too below
            $opts{before_section} = [
                'SEE ALSO',
                'HOMEPAGE',
            ];
        }

        $self->add_text_to_section($document, $sectcontent, $sectname, \%opts);
    }
    if ($found) {
        $self->log(["added POD sections from Rinci metadata for module '%s'", $filename]);
    }
}

sub _fmt_opt {
    my ($opt, $ospec) = @_;

    my @res;

    my $arg_spec = $ospec->{arg_spec};
    my $is_bool = $arg_spec->{schema} &&
        $arg_spec->{schema}[0] eq 'bool';
    my $show_default = exists($ospec->{default}) &&
        !$is_bool && !$ospec->{main_opt} && !$ospec->{is_alias};

    my $add_sum = '';
    if ($ospec->{is_base64}) {
        $add_sum = " (base64-encoded)";
    } elsif ($ospec->{is_json}) {
        $add_sum = " (JSON-encoded)";
    } elsif ($ospec->{is_yaml}) {
        $add_sum = " (YAML-encoded)";
    }

    $opt =~ s/(?P<name>--?.+?)(?P<val>=(?P<dest>[\w@-]+)|,|\z)/"B<" . $+{name} . ">" . ($+{dest} ? "=I<".$+{dest}.">" : $+{val})/eg;

    push @res, "=item $opt\n\n";

    push @res, "$ospec->{summary}$add_sum.\n\n" if $ospec->{summary};

    push @res, "Default value:\n\n ", dmp($ospec->{default}), "\n\n" if $show_default;

    if ($arg_spec->{schema} && $arg_spec->{schema}[1]{in} && !$ospec->{is_alias}) {
        push @res, "Valid values:\n\n ", dmp($arg_spec->{schema}[1]{in}), "\n\n";
    }

    if ($ospec->{main_opt}) {
        my $main_opt = $ospec->{main_opt};
        $main_opt =~ s/\s*,.+//;
        $main_opt =~ s/=.+//;
        push @res, "See C<$main_opt>.\n\n";
    } else {
        push @res, "$ospec->{description}\n\n" if $ospec->{description};
    }

    if (($ospec->{orig_opt} // '') =~ /\@/) {
        push @res, "Can be specified multiple times.\n\n";
    }

    join "", @res;
}

sub _list_config_params {
    my ($self, $clidocdata, $filter) = @_;

    my $opts = $clidocdata->{opts};
    my %param2opts;
    for (keys %$opts) {
        my $ospec = $opts->{$_};
        next unless $ospec->{common_opt} && $ospec->{common_opt_spec}{is_settable_via_config};
        next if $filter && !$filter->($ospec);
        my $oname = $ospec->{opt_parsed}{opts}[0];
        $oname = length($oname) > 1 ? "--$oname" : "-$oname";
        $param2opts{ $ospec->{common_opt} } = $oname;
    }
    for (keys %$opts) {
        my $ospec = $opts->{$_};
        next unless $ospec->{arg};
        next if $ospec->{main_opt};
        next if $filter && !$filter->($ospec);
        my $oname = $ospec->{opt_parsed}{opts}[0];
        $oname = length($oname) > 1 ? "--$oname" : "-$oname";
        my $confname = $param2opts{$_} ?
            "$ospec->{arg}.arg" : $ospec->{arg};
        $param2opts{$confname} = $oname;
    }
    \%param2opts;
}

sub _process_script {
    require Perinci::CmdLine::Dump;
    use experimental 'smartmatch';
    no warnings 'once';

    my ($self, $document, $input) = @_;

    local @INC = ("lib", @INC);

    my $filename = $input->{filename};

    my $res = $self->dump_perinci_cmdline_script($input);
    if ($res->[0] == 412) {
        $self->log_debug(["skipped file '%s' (%s)", $filename, $res->[1]]);
        return;
    } elsif ($res->[0] != 200) {
        die "Can't dump script: $res->[0] - $res->[1]";
    }

    my $cli = $res->[2];
    local %main::SPEC = %{ $res->[3]{'func.meta'} } if $res->[3]{'func.meta'};

    my $prog = $cli->{program_name};
    if (!$prog) {
        $prog = $filename;
        $prog =~ s!.+/!!;
    }

    # generate clidocdata(for all subcommands; if there is no subcommand then it
    # is stored in key '')
    my %metas; # key = subcommand name
    my %clidocdata; # key = subcommand name
    my %urls; # key = subcommand name

    {
        my $url = $cli->{url};
        $urls{''} = $url;
        my $res = $pa->request(meta => $url);
        die "Can't meta $url: $res->[0] - $res->[1]" unless $res->[0] == 200;
        my $meta = $res->[2];
        $metas{''} = $meta;

        $res = gen_cli_doc_data_from_meta(
            meta => $meta,
            meta_is_normalized => 0, # because riap client is specifically set not to normalize
            common_opts => $cli->{common_opts},
            per_arg_json => $cli->{per_arg_json},
            per_arg_yaml => $cli->{per_arg_yaml},
        );
        die "Can't gen_cli_doc_data_from_meta: $res->[0] - $res->[1]"
            unless $res->[0] == 200;
        $clidocdata{''} = $res->[2];

        if ($cli->{subcommands}) {
            if (ref($cli->{subcommands}) eq 'CODE') {
                die "Script '$filename': sorry, coderef 'subcommands' not ".
                    "supported";
            }
            for my $sc_name (keys %{ $cli->{subcommands} }) {
                my $sc_spec = $cli->{subcommands}{$sc_name};
                my $url = $sc_spec->{url};
                $urls{$sc_name} = $url;
                my $res = $pa->request(meta => $url);
                die "Can't meta $url (subcommand $sc_name): $res->[0] - $res->[1]"
                    unless $res->[0] == 200;
                my $meta = $res->[2];
                $metas{$sc_name} = $meta;
                $res = gen_cli_doc_data_from_meta(
                    meta => $meta,
                    meta_is_normalized => 0, # because riap client is specifically set not to normalize
                    common_opts => $cli->{common_opts},
                    per_arg_json => $cli->{per_arg_json},
                    per_arg_yaml => $cli->{per_arg_yaml},
                );
                die "Can't gen_cli_doc_data_from_meta (subcommand $sc_name): $res->[0] - $res->[1]"
                    unless $res->[0] == 200;
                $clidocdata{$sc_name} = $res->[2];
            }
        }
    }

    my $modified;

    # insert SYNOPSIS section
    {
        my @content;
        push @content, "Usage:\n\n";
        if ($cli->{subcommands}) {
            for my $sc_name (sort keys %clidocdata) {
                next unless length $sc_name;
                my $usage = $clidocdata{$sc_name}->{usage_line};
                $usage =~ s/\[\[prog\]\]/$prog $sc_name/;
                push @content, " % $usage\n";
            }
            push @content, "\n";
        } else {
            my $usage = $clidocdata{''}->{usage_line};
            $usage =~ s/\[\[prog\]\]/$prog/;
            push @content, " % $usage\n\n";
        }

        my @examples;
        for my $sc_name (sort keys %clidocdata) {
            next unless length $sc_name;
            my $i = 1;
            for my $eg (@{ $clidocdata{$sc_name}{examples} }) {
                # add pointer to subcommand, we need it later to show result
                $eg->{_sc_name} = $sc_name;
                $eg->{_i} = $i;
                push @examples, $eg;
                $i++;
            }
        }
        if (@examples) {
            push @content, "Examples:\n\n";
            for my $eg (@examples) {
                my $url = $urls{ $eg->{_sc_name} };
                my $meta = $metas{ $eg->{_sc_name} };
                push @content, "$eg->{summary}:\n\n" if $eg->{summary};
                my $cmdline = $eg->{cmdline};
                $cmdline =~ s/\[\[prog\]\]/$prog/;
                push @content, " % $cmdline\n";

                my $show_result;
              SHOW_RESULT:
                {
                    my $fres;
                    last unless $eg->{example_spec}{'x.doc.show_result'} // 1;

                    if ($eg->{example_spec}{src}) {
                        if ($eg->{example_spec}{src_plang} =~ /\A(bash)\z/) {
                            # write script to filesystem to a temporary file,
                            # execute it and get its output
                            my ($file) = grep { $_->name eq $input->{filename} }
                                @{ $input->{zilla}->files };
                            my ($fh, $filename) = tempfile();
                            print $fh $file->encoded_content;
                            close $fh;
                            my $cmdline = $eg->{cmdline};
                            $cmdline =~ s/\[\[prog\]\]/shell_quote($^X, "-Ilib", $filename)/e;
                            IPC::System::Options::system(
                                {shell => 0, capture_stdout => \$fres},
                                "bash", "-c", $cmdline);
                            if ($?) {
                                $self->log_fatal(["Example #%d (subcommand %s): cmdline %s: failed: %s", $eg->{_i}, $eg->{_sc_name}, $cmdline, explain_child_error()]);
                            }
                            if (my $max_lines = $eg->{example_spec}{'x.doc.max_result_lines'}) {
                                my @lines = split /^/, $fres;
                                if (@lines > $max_lines) {
                                    my $n = int($max_lines/2);
                                    splice @lines, $n, (@lines - $max_lines + 1), "...\n";
                                    $fres = join("", @lines);
                                }
                            }
                            $self->log_debug(["fres: %s", $fres]);
                        } else {
                            $self->log_debug(["Example #%d (subcommand %s) has src with unsupported src_plang ($eg->{srg_plang}), skipped showing result", $eg->{_i}, $eg->{_sc_name}]);
                            last SHOW_RESULT;
                        }
                    } else {
                        my $res;
                        if (exists $eg->{example_spec}{result}) {
                            $res = $eg->{example_spec}{result};
                        } else {
                            my %extra;
                            if ($eg->{example_spec}{argv}) {
                                $extra{argv} = $eg->{example_spec}{argv};
                            } elsif ($eg->{example_spec}{args}) {
                                $extra{args} = $eg->{example_spec}{args};
                            } else {
                                $self->log_debug(["Example #%d (subcommand %s) doesn't provide args/argv, skipped showing result", $eg->{_i}, $eg->{_sc_name}]);
                                last SHOW_RESULT;
                            }
                            $res = $pa->request(call => $url, \%extra);
                        }
                        my $format = $res->[3]{'cmdline.default_format'} // $cli->{default_format} // 'text-pretty';
                        require Perinci::Result::Format::Lite;
                        $fres = Perinci::Result::Format::Lite::format($res, $format);
                    }

                    $fres =~ s/^/ /gm;
                    push @content, $fres;
                    push @content, "\n";
                    $show_result = 1;
                } # SHOW_RESULT

                unless ($show_result) {
                    push @content, "\n";
                }
            }
        }
        last unless @content;

        $self->add_text_to_section(
            $document, join('', @content), 'SYNOPSIS',
            {
                ignore=>1,
                after_section => [
                    'VERSION',
                    'NAME',
                ],
                # just to make we don't put it too below
                before_section => [
                    'DESCRIPTION',
                    'SEE ALSO',
                ],
            });
        $modified++;
    }

    # insert DESCRIPTION section
    {
        last unless $metas{''}{description};

        my @content;
        push @content,
            Markdown::To::POD::markdown_to_pod($metas{''}{description});
        push @content, "\n\n";

        $self->add_text_to_section(
            $document, join('', @content), 'DESCRIPTION',
            {
                ignore=>1,
                after_section=>[
                    'SYNOPSIS',
                    'VERSION',
                    'NAME',
                ],
                # make sure we don't put it too below
                before_section => [
                    'SEE ALSO',
                ],
            });
        $modified++;
    }

    # insert SUBCOMMANDS section
    {
        last unless $cli->{subcommands};

        my @content;
        my %sc_spec_refs; # key=ref address, val=first subcommand name
        for my $sc_name (sort keys %clidocdata) {
            next unless length $sc_name;
            my $sc_spec = $cli->{subcommands}{$sc_name};

            my $spec_same_as;
            if (defined $sc_spec_refs{"$sc_spec"}) {
                $spec_same_as = $sc_spec_refs{"$sc_spec"};
            } else {
                $sc_spec_refs{"$sc_spec"} = $sc_name;
            }

            my $meta = $metas{$sc_name};
            push @content, "=head2 B<$sc_name>\n\n";

            # assumed alias because spec has been seen before
            if ($spec_same_as) {
                push @content, "Alias for C<$spec_same_as>.\n\n";
                next;
            }

            my $summary = $sc_spec->{summary} // $meta->{summary};
            push @content, "$summary.\n\n" if $summary;

            next if $sc_spec->{is_alias};

            my $description = $sc_spec->{description} // $meta->{description};
            if ($description) {
                push @content,
                    Markdown::To::POD::markdown_to_pod($description);
                push @content, "\n\n";
            }
        }

        $self->add_text_to_section(
            $document, join('', @content), 'SUBCOMMANDS',
            {
                ignore=>1,
                after_section => [
                    'DESCRIPTION',
                    'SYNOPSIS',
                ],
                # make sure we don't put it too below
                before_section => [
                    'OPTIONS',
                    'SEE ALSO',
                ],
            });
        $modified++;
    }

    my @sc_names = grep { length } sort keys %clidocdata;

    # insert OPTIONS section
    {
        my @content;
        push @content, "C<*> marks required options.\n\n";

        if ($cli->{subcommands}) {

            # currently categorize by subcommand instead of category

            my $check_common_arg = sub {
                my ($opts, $name) = @_;
                return 1 if 'common' ~~ @{ $opts->{$name}{tags} // []};
                return 1 if !$opts->{$name}{arg};
                0;
            };

            # first display options tagged with 'common' as well as common
            # option (non-function argument option, like --format or
            # --log-level). these are supposed to be the same across
            # subcommands.
            {
                use experimental 'smartmatch';
                my $opts = $clidocdata{ $sc_names[0] }{opts};
                my @opts = sort {
                    (my $a_without_dash = $a) =~ s/^-+//;
                    (my $b_without_dash = $b) =~ s/^-+//;
                    lc($a) cmp lc($b);
                } grep {$check_common_arg->($opts, $_)} keys %$opts;
                push @content, "=head2 Common options\n\n";
                push @content, "=over\n\n";
                for (@opts) {
                    push @content, _fmt_opt($_, $opts->{$_});
                }
                push @content, "=back\n\n";
            }

            # display each subcommand's options (without the options tagged as
            # 'common')
            my %sc_spec_refs;
            for my $sc_name (@sc_names) {
                my $sc_spec = $cli->{subcommands}{$sc_name};

                my $spec_same_as;
                if (defined $sc_spec_refs{"$sc_spec"}) {
                    $spec_same_as = $sc_spec_refs{"$sc_spec"};
                } else {
                    $sc_spec_refs{"$sc_spec"} = $sc_name;
                }
                next if defined $spec_same_as;
                next if $sc_spec->{is_alias};

                my $opts = $clidocdata{$sc_name}{opts};
                my @opts = sort {
                    (my $a_without_dash = $a) =~ s/^-+//;
                    (my $b_without_dash = $b) =~ s/^-+//;
                    lc($a) cmp lc($b);
                } grep {!$check_common_arg->($opts, $_)} keys %$opts;
                next unless @opts;
                push @content, "=head2 Options for subcommand $sc_name\n\n";
                push @content, "=over\n\n";
                for (@opts) {
                    push @content, _fmt_opt($_, $opts->{$_});
                }
                push @content, "=back\n\n";
            }
        } else {
            my $opts = $clidocdata{''}{opts};
            # find all the categories
            my %options_by_cat; # val=[options...]
            for my $optkey (keys %$opts) {
                for my $cat (@{ $opts->{$optkey}{categories} }) {
                    push @{ $options_by_cat{$cat} }, $optkey;
                }
            }
            my $cats_spec = $clidocdata{''}{option_categories};
            for my $cat (sort {
                ($cats_spec->{$a}{order} // 50) <=> ($cats_spec->{$b}{order} // 50)
                    || $a cmp $b }
                             keys %options_by_cat) {
                push @content, "=head2 $cat\n\n"
                    unless keys(%options_by_cat) == 1;

                my @opts = sort {
                    (my $a_without_dash = $a) =~ s/^-+//;
                    (my $b_without_dash = $b) =~ s/^-+//;
                    lc($a) cmp lc($b);
                } @{ $options_by_cat{$cat} };
                push @content, "=over\n\n";
                for (@opts) {
                    push @content, _fmt_opt($_, $opts->{$_});
                }
                push @content, "=back\n\n";
            }
        }

        $self->add_text_to_section(
            $document, join('', @content), 'OPTIONS',
            {
                ignore => 1,
                after_section => [
                    'SUBCOMMANDS',
                    'DESCRIPTION',
                ],
                # make sure we don't put it too below
                before_section => [
                    'COMPLETION',
                    'ENVIRONMENT',
                    'CONFIGURATION FILE',
                    'HOMEPAGE',
                    'SEE ALSO',
                    'AUTHOR',
                ],
            });
        $modified++;
    }

    # insert ENVIRONMENT section
    {
        # workaround because currently the dumped object does not contain all
        # attributes in the hash (Moo/Mo issue?), we need to access the
        # attribute accessor method first to get them recorded in the hash. this
        # will be fixed in the dump module in the future.
        local $0 = $filename;
        local @INC = ("lib", @INC);
        eval "use " . ref($cli) . "()";
        die if $@;

        last unless $cli->read_env;
        #$self->log_debug(["skipped file %s (script does not read env)", $filename]);

        my $env_name = $cli->env_name;
        if (!$env_name) {
            $env_name = uc($prog);
            $env_name =~ s/\W+/_/g;
        }

        my @content;
        push @content, "=head2 ", $env_name, " => str\n\n";
        push @content, "Specify additional command-line options\n\n";

        $self->add_text_to_section(
            $document, join('', @content), 'ENVIRONMENT',
            {
                after_section => [
                    'OPTIONS',
                ],
                # make sure we don't put it too below
                before_section => [
                    'HOMEPAGE',
                    'SEE ALSO',
                ],
            });
        $modified++;
    }

    # insert CONFIGURATION FILE & FILES sections
    {
        # workaround because currently the dumped object does not contain all
        # attributes in the hash (Moo/Mo issue?), we need to access the
        # attribute accessor method first to get them recorded in the hash. this
        # will be fixed in the dump module in the future.
        local $0 = $filename;
        local @INC = ("lib", @INC);
        eval "use " . ref($cli) . "()";
        die if $@;

        last unless $cli->read_config;

        my $config_filenames = [];
        my $config_dirs;
        my @files;
        my @sections;

        # FILES section
        {
            my @content;
            if (my $cfns = $cli->config_filename) {
                for my $cfn (ref($cfns) eq 'ARRAY' ? @$cfns : $cfns) {
                    if (ref($cfn) eq 'HASH') {
                        push @$config_filenames, $cfn;
                    } else {
                        push @$config_filenames, {filename=>$cfn};
                    }
                }
            } elsif ($cli->program_name) {
                push @$config_filenames,
                    {filename => $cli->program_name . ".conf"};
            } else {
                push @$config_filenames,
                    {filename => $prog . ".conf"};
            }
            $config_dirs = $cli->{config_dirs} // ['~/.config', '~', '/etc'];

            for my $config_dir (@$config_dirs) {
                for my $cfn (@$config_filenames) {
                    my $p = "$config_dir/$cfn->{filename}";
                    push @files, $p;
                    push @sections, $cfn->{section};
                    push @content, "$p\n\n";
                }
            }

            $self->add_text_to_section(
                $document, join('', @content), 'FILES',
                {
                    after_section => 'ENVIRONMENT',
                    # make sure we don't put it too below
                    before_section => [
                        'SEE ALSO',
                    ],
                });
        }

        # CONFIGURATION FILE section
        {
            my @content;

            push @content, (
                "This script can read configuration files. Configuration files are in the format of L<IOD>, which is basically INI with some extra features.\n\n",

                "By default, these names are searched for configuration filenames (can be changed using C<--config-path>): ",
                (map {(
                    # add those pesky comma's/'or's (for two items: A or B, for
                    # >2 items: A, B, or C)
                    (@files == 2 && $_ == 1 ? " or" : ""),
                    (@files >  2 && $_ == $#files ? ", or" : ""),
                    (@files >  2 && $_ >  0 && $_ < $#files ? "," : ""),
                    ($_ > 0 ? " " : ""),
                    "F<$files[$_]>",
                    (defined($sections[$_]) ? " (under the section C<$sections[$_]>)" : ""),
                )} 0..$#files), ".\n\n",

                "All found files will be read and merged.", "\n\n",

                "To disable searching for configuration files, pass C<--no-config>.\n\n",

                ($cli->{subcommands} ? ("To put configuration for a certain subcommand only, use a section name like C<[subcommand=NAME]> or C<[SOMESECTION subcommand=NAME]>.\n\n") : ()),

                "You can put multiple profiles in a single file by using section names like C<[profile=SOMENAME]> or C<[SOMESECTION profile=SOMENAME]>",
                ($cli->{subcommands} ? " or C<[subcommand=SUBCOMMAND_NAME profile=SOMENAME]> or C<[SOMESECTION subcommand=SUBCOMMAND_NAME profile=SOMENAME]>":""), ". ",
                "Those sections will only be read if you specify the matching C<--config-profile SOMENAME>.", "\n\n",

                "You can also put configuration for multiple programs inside a single file, and use filter C<program=NAME> in section names, e.g. C<[program=NAME ...]> or C<[SOMESECTION program=NAME]>. ",
                "The section will then only be used when the reading program matches.\n\n",

                "Finally, you can filter a section by environment variable using the filter C<env=CONDITION> in section names. ",
                "For example if you only want a section to be read if a certain environment variable is true: C<[env=SOMEVAR ...]> or C<[SOMESECTION env=SOMEVAR ...]>. ",
                "If you only want a section to be read when the value of an environment variable has value equals something: C<[env=HOSTNAME=blink ...]> or C<[SOMESECTION env=HOSTNAME=blink ...]>. ",
                "If you only want a section to be read when the value of an environment variable does not equal something: C<[env=HOSTNAME!=blink ...]> or C<[SOMESECTION env=HOSTNAME!=blink ...]>. ",
                "If you only want a section to be read when an environment variable contains something: C<[env=HOSTNAME*=server ...]> or C<[SOMESECTION env=HOSTNAME*=server ...]>. ",
                "Note that currently due to simplistic parsing, there must not be any whitespace in the value being compared because it marks the beginning of a new section filter or section name.\n\n",

                "List of available configuration parameters:\n\n",
            );

            if ($cli->{subcommands}) {
                # first list the options tagged with 'common' and common options
                # (non-function argument options, like --format or --log-level)
                # which are supposed to be the same across subcommands.
                push @content, "=head2 Common for all subcommands\n\n";
                my $param2opts = $self->_list_config_params(
                    $clidocdata{$sc_names[0]},
                    sub { 'common' ~~ @{ $_[0]->{tags} // []} || !$_[0]->{arg} });
                for (sort keys %$param2opts) {
                    push @content, " $_ (see $param2opts->{$_})\n";
                }
                push @content, "\n";

                # now list the options for each subcommand
                for my $sc_name (@sc_names) {
                    my $sc_spec = $cli->{subcommands}{$sc_name};
                    next if $sc_spec->{is_alias};
                    push @content, "=head2 Configuration for subcommand '$sc_name'\n\n";
                    $param2opts = $self->_list_config_params(
                        $clidocdata{$sc_name},
                        sub { !('common' ~~ @{ $_[0]->{tags} // []}) && $_[0]->{arg} });
                    for (sort keys %$param2opts) {
                        push @content, " $_ (see $param2opts->{$_})\n";
                    }
                    push @content, "\n";
                }
            } else {
                my $param2opts = $self->_list_config_params($clidocdata{''});
                for (sort keys %$param2opts) {
                    push @content, " $_ (see $param2opts->{$_})\n";
                }
                push @content, "\n";
            }

            $self->add_text_to_section(
                $document, join('', @content), 'CONFIGURATION FILE',
                {
                    before_section=>[
                        'FILES',
                    ],
                });
        }

        $modified++;
    }

    # insert SEE ALSO section
    {
        my @content;

        my %seen_urls;
        for my $sc_name (sort keys %clidocdata) {
            my $meta = $metas{$sc_name};
            next unless $meta->{links};
            for my $link0 (@{ $meta->{links} }) {
                my $link = ref($link0) ? $link0 : {url=>$link0};
                my $url = $link->{url};
                next if $seen_urls{$url}++;
                if ($url =~ s!^(pm|pod|prog):(//?)?!!) {
                    push @content, "L<$url>.";
                } else {
                    push @content, "L<$url>.";
                }
                if ($link->{summary}) {
                    push @content, " $link->{summary}";
                    push @content, "." unless $link->{summary} =~ /\.$/;
                }
                push @content, " " .
                    Markdown::To::POD::markdown_to_pod($link->{description})
                      if $link->{description};
                push @content, "\n\n";
            }
        }

        last unless @content;

        $self->add_text_to_section(
            $document, join('', @content), 'SEE ALSO',
            {
            });
        $modified++;
    }

    if ($modified) {
        $self->log(["added POD sections from Rinci metadata for script '%s'", $filename]);
    }
}

sub weave_section {
    my ($self, $document, $input) = @_;

    my $filename = $input->{filename};

    if (defined $self->exclude_files) {
        my $re = $self->exclude_files;
        eval { $re = qr/$re/ };
        $@ and die "Invalid regex in exclude_files: $re";
        if ($filename =~ $re) {
            $self->log_debug(["skipped file '%s' (matched exclude_files)", $filename]);
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

=head2 For modules

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

=head2 For Perinci::CmdLine-based CLI script

For scripts using L<Perinci::CmdLine> (or its variant ::Any and ::Lite), the
following are inserted:

=over

=item * SYNOPSIS

If the script's POD does not yet have this section, this section will be added
containing the usage line of the script.

=item * DESCRIPTION

If the script's POD does not already have this section, and if the script does
not have subcommands, description from function metadata will be inserted here,
if any.

=item * SUBCOMMANDS

If the script's POD does not already have his section, and if the script has
subcommands, then each subcommand will be listed here along with its summary and
description.

=item * OPTIONS

If the script's POD does not already have his section, command-line options for
the script will be listed here. If script has subcommands, the options will be
categorized per subcommand.

=item * CONFIGURATION

If the script's POD does not already have his section, general information about
configuration files and available configuration parameters will be listed here.
If script has subcommands, the parameters will be categorized per subcommand.

=item * FILES

Configuration files read by script will be listed here.

=back

To get Perinci::CmdLine object information (which contains the URL of the Rinci
function, or the list of subcommands, among others), the script is run with a
patched C<run()> that will dump the content of the object and exit immediately,
so the plugin can inspect it.

Caveats: 1) Function used by the script must reside in the module, not embedded
inside the script itself, otherwise it will not be readable by the plugin. 2)
Coderef C<subcommands> is not supported.

To exclude a script from being processed, you can also put C<# NO_PWP_RINCI> in
the script.


=head1 SEE ALSO

L<Pod::Weaver>
