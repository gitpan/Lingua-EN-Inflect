use ExtUtils::MakeMaker;
# See lib/ExtUtils/MakeMaker.pm for details of how to influence
# the contents of the Makefile that is written.
WriteMakefile(
    'NAME'	=> 'Inflect',
    'VERSION_FROM' => 'bin/Lingua/EN/Inflect.pm', # finds $VERSION
);
