use 5.010;
use warnings;
use Test::More 'no_plan';

my $parser = do{
    use Regexp::Grammars;
    qr{
        <file>

        <objrule: file>
            <[element]>*

        <objrule: element>
            <command> | <literal>

        <objrule: command>
            \\  <name=literal>  <options>?  <args>?

        <objrule: options>
            \[  <[option]> ** (,)  \]
        
        <objrule: args>
            \{  <[element]>*  \}

        <objrule: option>
            [^][\$&%#_{}~^\s,]+

        <objrule: literal>
            [^][\$&%#_{}~^\s]+

    }xms
};

my $target = {
  '' => '\\documentclass[a4paper,11pt]{article}
\\usepackage{latexsym}
\\author{D. Conway}
\\title{Parsing \\LaTeX{}}
\\begin{document}
\\maketitle
\\tableofcontents
\\section{Description}
...is easy \\footnote{But not \\emph{necessarily} simple}.
\\end{document}',
  'file' => bless(
    {
      '' => '\\documentclass[a4paper,11pt]{article}
\\usepackage{latexsym}
\\author{D. Conway}
\\title{Parsing \\LaTeX{}}
\\begin{document}
\\maketitle
\\tableofcontents
\\section{Description}
...is easy \\footnote{But not \\emph{necessarily} simple}.
\\end{document}',
      'element' => [
        bless(
          {
            ''    => '\\documentclass[a4paper,11pt]{article}',
            'command' => bless(
              {
                '' =>
                  '\\documentclass[a4paper,11pt]{article}',
                'options' => bless(
                  {
                    ''     => '[a4paper,11pt]',
                    'option' => [
                      bless(
                        { '' => 'a4paper' }, 'option'
                      ),
                      bless(
                        { '' => '11pt' }, 'option'
                      )
                    ]
                  }, 'options'
                ),
                'name' => bless(
                  { '' => 'documentclass' }, 'literal'
                ),
                'args' => bless(
                  {
                    ''    => '{article}',
                    'element' => [
                      bless(
                        {
                          'literal' => bless(
                            { '' => 'article' },
                            'literal'
                          ),
                          '' => 'article'
                        }, 'element'
                      )
                    ]
                  }, 'args'
                )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            '' => '
\\usepackage{latexsym}',
            'command' => bless(
              {
                ''   => '\\usepackage{latexsym}',
                'name' => bless(
                  { '' => 'usepackage' }, 'literal'
                ),
                'args' => bless(
                  {
                    ''    => '{latexsym}',
                    'element' => [
                      bless(
                        {
                          'literal' => bless(
                            { '' => 'latexsym' },
                            'literal'
                          ),
                          '' => 'latexsym'
                        }, 'element'
                      )
                    ]
                  }, 'args'
                )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            '' => '
\\author{D. Conway}',
            'command' => bless(
              {
                ''   => '\\author{D. Conway}',
                'name' =>
                  bless( { '' => 'author' }, 'literal' ),
                'args' => bless(
                  {
                    ''    => '{D. Conway}',
                    'element' => [
                      bless(
                        {
                          'literal' => bless(
                            { '' => 'D.' },
                            'literal'
                          ),
                          '' => 'D.'
                        }, 'element'
                      ),
                      bless(
                        {
                          'literal' => bless(
                            { '' => 'Conway' },
                            'literal'
                          ),
                          '' => ' Conway'
                        }, 'element'
                      )
                    ]
                  }, 'args'
                )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            '' => '
\\title{Parsing \\LaTeX{}}',
            'command' => bless(
              {
                ''   => '\\title{Parsing \\LaTeX{}}',
                'name' =>
                  bless( { '' => 'title' }, 'literal' ),
                'args' => bless(
                  {
                    ''    => '{Parsing \\LaTeX{}}',
                    'element' => [
                      bless(
                        {
                          'literal' => bless(
                            { '' => 'Parsing' },
                            'literal'
                          ),
                          '' => 'Parsing'
                        }, 'element'
                      ),
                      bless(
                        {
                          ''    => ' \\LaTeX{}',
                          'command' => bless(
                            {
                              '' => '\\LaTeX{}',
                              'name' => bless(
                                {
                                  '' =>
                                    'LaTeX'
                                }, 'literal'
                              ),
                              'args' => bless(
                                {
                                  '' => '{}'
                                }, 'args'
                              )
                            }, 'command'
                          )
                        }, 'element'
                      )
                    ]
                  }, 'args'
                )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            '' => '
\\begin{document}',
            'command' => bless(
              {
                ''   => '\\begin{document}',
                'name' =>
                  bless( { '' => 'begin' }, 'literal' ),
                'args' => bless(
                  {
                    ''    => '{document}',
                    'element' => [
                      bless(
                        {
                          'literal' => bless(
                            { '' => 'document' },
                            'literal'
                          ),
                          '' => 'document'
                        }, 'element'
                      )
                    ]
                  }, 'args'
                )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            '' => '
\\maketitle
',
            'command' => bless(
              {
                '' => '\\maketitle
',
                'name' =>
                  bless( { '' => 'maketitle' }, 'literal' )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            '' => '\\tableofcontents
',
            'command' => bless(
              {
                '' => '\\tableofcontents
',
                'name' => bless(
                  { '' => 'tableofcontents' }, 'literal'
                )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            ''    => '\\section{Description}',
            'command' => bless(
              {
                ''   => '\\section{Description}',
                'name' =>
                  bless( { '' => 'section' }, 'literal' ),
                'args' => bless(
                  {
                    ''    => '{Description}',
                    'element' => [
                      bless(
                        {
                          'literal' => bless(
                            {
                              '' => 'Description'
                            }, 'literal'
                          ),
                          '' => 'Description'
                        }, 'element'
                      )
                    ]
                  }, 'args'
                )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            'literal' => bless( { '' => '...is' }, 'literal'),
            '' => '
...is'
          }, 'element'
        ),
        bless(
          {
            'literal' => bless( { '' => 'easy' }, 'literal' ),
            ''    => ' easy'
          },
          'element'
        ),
        bless(
          {
            '' => ' \\footnote{But not \\emph{necessarily} simple}',
            'command' => bless(
              {
                '' =>
                  '\\footnote{But not \\emph{necessarily} simple}',
                'name' => bless( { '' => 'footnote' }, 'literal' ),
                'args' => bless(
                  {
                    '' => '{But not \\emph{necessarily} simple}',
                    'element' => [
                      bless(
                        {
                          'literal' => bless( { '' => 'But' }, 'literal'),
                          '' => 'But'
                        }, 'element'
                      ),
                      bless(
                        {
                          'literal' => bless( { '' => 'not' }, 'literal'),
                          '' => ' not'
                        },
                        'element'
                      ),
                      bless(
                        {
                          '' => ' \\emph{necessarily}',
                          'command' => bless(
                            {
                              '' => '\\emph{necessarily}',
                              'name' => bless( { '' => 'emph' }, 'literal'
                              ),
                              'args' => bless(
                                {
                                  '' =>
                                    '{necessarily}',
                                  'element' => [
                                    bless(
                                      {
                                        'literal' => bless (
                                          {
                                            '' => 'necessarily'
                                          }, 'literal'
                                          )
                                        ,
                                        '' => 'necessarily'
                                      }, 'element'
                                    )
                                    ]
                                }, 'args'
                              )
                            }, 'command'
                          )
                        }, 'element'
                      ),
                      bless(
                        {
                          'literal' => bless(
                            { '' => 'simple' },
                            'literal'
                          ),
                          '' => ' simple'
                        }, 'element'
                      )
                    ]
                  }, 'args'
                )
              }, 'command'
            )
          }, 'element'
        ),
        bless(
          {
            'literal' => bless( { '' => '.' }, 'literal' ),
            ''    => '.'
          }, 'element'
        ),
        bless(
          {
            '' => '
\\end{document}',
            'command' => bless(
              {
                ''   => '\\end{document}',
                'name' => bless( { '' => 'end' }, 'literal' ),
                'args' => bless(
                  {
                    ''    => '{document}',
                    'element' => [
                      bless(
                        {
                          'literal' => bless( { '' => 'document' }, 'literal'
                          ),
                          '' => 'document'
                        }, 'element'
                      )
                    ]
                  }, 'args'
                )
              }, 'command'
            )
          }, 'element'
        )
      ]
    }, 'file'
  )
};

my $input = do{ local $/; <DATA>};
chomp $input;
my $original_input = $input;

ok +($input =~ $parser)    => 'Matched';
is_deeply \%/, $target     => 'Returned correct data structure';
is $/{""}, $original_input => 'Captured entire text';

__DATA__
\documentclass[a4paper,11pt]{article}
\usepackage{latexsym}
\author{D. Conway}
\title{Parsing \LaTeX{}}
\begin{document}
\maketitle
\tableofcontents
\section{Description}
...is easy \footnote{But not \emph{necessarily} simple}.
\end{document}
