package Recordstorebot;
 
# COmpletely ripped off from the Chatbot::Eliza program by John Nolan
# which was Copyright (c) 1997-2003 John Nolan. 

require 5.003; 
use strict;
use vars qw($VERSION @ISA $AUTOLOAD); 

# thor added - needed for Lastfm WS
use Carp;
use Digest::MD5 qw(md5 md5_hex md5_base64);
use REST::Client;
use XML::Simple;

# Lastfm WS variables 
my $rooturl = "http://ws.audioscrobbler.com/2.0/";
my $api_key = '14a64b148e0bb4256d1f863dfd9236da';

$VERSION = '1.04';
sub Version { $VERSION; }

my %fields = (
	name 		=> 'BobbyBot',
	scriptfile	=> '',
	debug 		=> 0,
	debug_text	=> '',
	transform_text	=> '',
	prompts_on	=> 1,
	memory_on       => 1,
	botprompt	=> '',
	userprompt	=> '',

	myrand          => 
			sub { my $N = defined $_[0] ? $_[0] : 1;  rand($N); },

	keyranks	=> undef,
	decomplist	=> undef,
	reasmblist	=> undef,
	reasmblist_for_memory	=> undef,

	pre		=> undef,
	post		=> undef,
	synon		=> undef,
	initial		=> undef,
	final		=> undef,
	quit		=> undef, 

	max_memory_size			=> 5,
	likelihood_of_using_memory	=> 1,
	memory				=> undef,
);


sub new {
	my ($that,$name,$scriptfile) = @_;
	my $class = ref($that) || $that;
	my $self = {
		_permitted => \%fields,
		%fields,
	};
	bless $self, $class;
	$self->_initialize($name,$scriptfile);
	return $self;
} # end method new

sub _initialize {
	my ($self,$param1,$param2) = @_;

	if (defined $param1 and ref $param1 eq "HASH") {

		# Allow the calling program to pass in intial parameters
		# as an anonymous hash
		map { $self->{$_} = $param1->{$_}; } keys %$param1;

		$self->parse_script_data( $self->{scriptfile} );

	} else {
		$self->name($param1) if $param1;
		$self->parse_script_data($param2);
	} 

	# Initialize the memory array ref at instantiation time,
	# rather than at class definition time. 
	# (THANKS to Randal Schwartz and Robert Chin for fixing this bug.) 
	#
	$self->{memory} = [];
}

sub AUTOLOAD {
	my $self = shift;
	my $class = ref($self) || croak "$self is not an object : $!\n";
	my $field = $AUTOLOAD;
	$field =~ s/.*://; # Strip fully-qualified portion

	unless (exists $self->{"_permitted"}->{$field} ) {
		croak "Can't access `$field' field in object of class $class : $!\n";
	}

	if (@_) {
		return $self->{$field} = shift;
	} else {
		return $self->{$field};
	}
} # end method AUTOLOAD


####################################################################
# --- command_interface ---

=head2 command_interface()

    $chatterbot->command_interface;

command_interface() opens an interactive session with 
the Eliza object, just like the original Eliza program.

If you want to design your own session format, then 
you can write your own while loop and your own functions
for prompting for and reading user input, and use the 
transform() method to generate Eliza's responses. 
(I<Note>: you do not need to invoke preprocess()
and postprocess() directly, because these are invoked
from within the transform() method.) 

But if you're lazy and you want to skip all that,
then just use command_interface().  It's all done for you. 

During an interactive session invoked using command_interface(),
you can enter the word "debug" to toggle debug mode on and off.
You can also enter the keyword "memory" to invoke the _debug_memory()
method and print out the contents of the Eliza instance's memory.

=cut

sub command_interface {
	my $self = shift;
	my ($user_input, $previous_user_input, $reply);

	$user_input = "";

	$self->botprompt($self->name . ":\t");	# Eliza's prompt 
	$self->userprompt("you:\t");     	# User's prompt

	# Seed the random number generator.
	srand( time() ^ ($$ + ($$ << 15)) );  

	# Print the Eliza prompt
	print $self->botprompt if $self->prompts_on;

	# Print an initial greeting
	print "$self->{initial}->[ int &{$self->{myrand}}( scalar @{ $self->{initial} } ) ]\n";


	###################################################################
	# command loop.  This loop should go on forever,
	# until we explicity break out of it. 
	#
	while (1) {

		print $self->userprompt if $self->prompts_on;

		$previous_user_input = $user_input;
		chomp( $user_input = <STDIN> ); 


		# If the user wants to quit,
		# print out a farewell and quit.
		if ($self->_testquit($user_input) ) {
			$reply = "$self->{final}->[ int &{$self->{myrand}}( scalar @{$self->{final}} ) ]";
			print $self->botprompt if $self->prompts_on;
			print "$reply\n";
			last;
		} 

		# If the user enters the word "debug",
		# then toggle on/off this Eliza's debug output.
		if ($user_input eq "debug") {
			$self->debug( ! $self->debug );
			$user_input = $previous_user_input;
		}

		# If the user enters the word "memory",
		# then use the _debug_memory method to dump out
		# the current contents of Eliza's memory
		if ($user_input eq "memory" or $user_input eq "debug memory") {
			print $self->_debug_memory();
			redo;
		}

		# If the user enters the word "debug that",
		# then dump out the debugging of the 
		# most recent call to transform.  
		if ($user_input eq "debug that") {
			print $self->debug_text();
			redo;
		}

		# Invoke the transform method
		# to generate a reply.
		$reply = $self->transform( $user_input );


		# Print out the debugging text if debugging is set to on.
		# This variable should have been set by the transform method.
		print $self->debug_text if $self->debug;

		# Print the actual reply
		print $self->botprompt if $self->prompts_on;
		print "$reply\n";

	} # End UI command loop.  


} # End method command_interface


####################################################################
# --- preprocess ---

=head2 preprocess()

    $string = preprocess($string);

preprocess() applies simple substitution rules to the input string.
Mostly this is to catch varieties in spelling, misspellings,
contractions and the like.  

preprocess() is called from within the transform() method.  
It is applied to user-input text, BEFORE any processing,
and before a reassebly statement has been selected. 

It uses the array C<%pre>, which is created 
during the parse of the script.

=cut

sub preprocess {
	my ($self,$string) = @_;

	my ($i, @wordsout, @wordsin, $keyword);

	@wordsout = @wordsin = split / /, $string;

	WORD: for ($i = 0; $i < @wordsin; $i++) {
		foreach $keyword (keys %{ $self->{pre} }) {
			if ($wordsin[$i] =~ /\b$keyword\b/i ) {
				($wordsout[$i] = $wordsin[$i]) =~ s/$keyword/$self->{pre}->{$keyword}/ig;
				next WORD;
			}
		}
	}
	return join ' ', @wordsout;
}


####################################################################
# --- postprocess ---

=head2 postprocess()

    $string = postprocess($string);

postprocess() applies simple substitution rules to the 
reassembly rule.  This is where all the "I"'s and "you"'s 
are exchanged.  postprocess() is called from within the
transform() function.

It uses the array C<%post>, created 
during the parse of the script.

=cut

sub postprocess {
	my ($self,$string) = @_;

	my ($i, @wordsout, @wordsin, $keyword);

	@wordsin = @wordsout = split (/ /, $string);

	WORD: for ($i = 0; $i < @wordsin; $i++) {
		foreach $keyword (keys %{ $self->{post} }) {
			if ($wordsin[$i] =~ /\b$keyword\b/i ) {
				($wordsout[$i] = $wordsin[$i]) =~ s/$keyword/$self->{post}->{$keyword}/ig;
				next WORD;
			}
		}
	}
	return join ' ', @wordsout;
}

####################################################################
# --- _testquit ---

=head2 _testquit()

     if ($self->_testquit($user_input) ) { ... }

_testquit() detects words like "bye" and "quit" and returns
true if it finds one of them as the first word in the sentence. 

These words are listed in the script, under the keyword "quit". 

=cut

sub _testquit {
	my ($self,$string) = @_;

	my ($quitword, @wordsin);

	foreach $quitword (@{ $self->{quit} }) {
		return 1 if ($string =~ /\b$quitword\b/i ) ;
	}
}

####################################################################
# --- _debug_memory ---

=head2 _debug_memory()

     $self->_debug_memory()

_debug_memory() is a special function which returns
the contents of Eliza's memory stack. 


=cut

sub _debug_memory {

	my ($self) = @_;

	my $string = "\t";           
	$string .= $#{ $self->memory } + 1;
	$string .= " item(s) in memory stack:\n";

	# [THANKS to Roy Stephan for helping me adjust this bit]
	#
	foreach (@{ $self->memory } ) { 

		my $line = $_; 
		$string .= sprintf "\t\t->$line\n" ;
	};

	return $string;
}

####################################################################
# --- transform ---

=head2 transform()

    $reply = $chatterbot->transform( $string, $use_memory );

transform() applies transformation rules to the user input
string.  It invokes preprocess(), does transformations, 
then invokes postprocess().  It returns the tranformed 
output string, called C<$reasmb>.  

The algorithm embedded in the transform() method has three main parts:

=over

=item 1 

Search the input string for a keyword.

=item 2 

If we find a keyword, use the list of decomposition rules
for that keyword, and pattern-match the input string against
each rule.

=item 3

If the input string matches any of the decomposition rules,
then randomly select one of the reassembly rules for that
decomposition rule, and use it to construct the reply. 

=back

transform() takes two parameters.  The first is the string we want
to transform.  The second is a flag which indicates where this sting
came from.  If the flag is set, then the string has been pulled
from memory, and we should use reassembly rules appropriate
for that.  If the flag is not set, then the string is the most
recent user input, and we can use the ordinary reassembly rules. 

The memory flag is only set when the transform() function is called 
recursively.  The mechanism for setting this parameter is
embedded in the transoform method itself.  If the flag is set
inappropriately, it is ignored.  

=cut

sub transform{
	my ($self,$string,$use_memory) = @_;

	# Initialize the debugging text buffer.
	$self->debug_text('');

	$self->debug_text(sprintf "\t[Pulling string \"$string\" from memory.]\n")
		if $use_memory;

	my ($i, @string_parts, $string_part, $rank, $goto, $reasmb, $keyword, 
		$decomp, $this_decomp, $reasmbkey, @these_reasmbs,
		@decomp_matches, $synonyms, $synonym_index);

	# Default to a really low rank. 
	$rank   = -2;
	$reasmb = "";
	$goto   = "";

	# First run the string through the preprocessor.  
	$string = $self->preprocess( $string );

	# Convert punctuation to periods.  We will assume that commas
	# and certain conjunctions separate distinct thoughts/sentences.  
	$string =~ s/[?!,]/./g;
	$string =~ s/but/./g;   #   Yikes!  This is English-specific. 

	# Split the string by periods into an array
	@string_parts = split /\./, $string ;

	# Examine each part of the input string in turn.
	STRING_PARTS: foreach $string_part (@string_parts) {

	# Run through the whole list of keywords.  
	KEYWORD: foreach $keyword (keys %{ $self->{decomplist} }) {

		# Check to see if the input string contains a keyword
		# which outranks any we have found previously
		# (On first loop, rank is set to -2.)
		if ( ($string_part =~ /\b$keyword\b/i or $keyword eq $goto) 
		     and 
		     $rank < $self->{keyranks}->{$keyword}  
		   ) 
		{
			# If we find one, then set $rank to equal 
			# the rank of that keyword. 
			$rank = $self->{keyranks}->{$keyword};

			$self->debug_text($self->debug_text . sprintf "\t$rank> $keyword");

			# Now let's check all the decomposition rules for that keyword. 
			DECOMP: foreach $decomp (@{ $self->{decomplist}->{$keyword} }) {

				# Change '*' to '\b(.*)\b' in this decomposition rule,
				# so we can use it for regular expressions.  Later, 
				# we will want to isolate individual matches to each wildcard. 
				($this_decomp = $decomp) =~ s/\s*\*\s*/\\b\(\.\*\)\\b/g;

				# If this docomposition rule contains a word which begins with '@', 
				# then the script also contained some synonyms for that word.  
				# Find them all using %synon and generate a regular expression 
				# containing all of them. 
				if ($this_decomp =~ /\@/ ) {
					($synonym_index = $this_decomp) =~ s/.*\@(\w*).*/$1/i ;
					$synonyms = join ('|', @{ $self->{synon}->{$synonym_index} });
					$this_decomp =~ s/(.*)\@$synonym_index(.*)/$1($synonym_index\|$synonyms)$2/g;
				}

				$self->debug_text($self->debug_text .  sprintf "\n\t\t: $decomp");

				# Using the regular expression we just generated, 
				# match against the input string.  Use empty "()"'s to 
				# eliminate warnings about uninitialized variables. 
				if ($string_part =~ /$this_decomp()()()()()()()()()()/i) {

					# If this decomp rule matched the string, 
					# then create an array, so that we can refer to matches
					# to individual wildcards.  Use '0' as a placeholder
					# (we don't want to refer to any "zeroth" wildcard).
					@decomp_matches = ("0", $1, $2, $3, $4, $5, $6, $7, $8, $9); 
					$self->debug_text($self->debug_text . sprintf " : @decomp_matches\n");

					# Using the keyword and the decomposition rule,
					# reconstruct a key for the list of reassamble rules.
					$reasmbkey = join ($;,$keyword,$decomp);

					# Get the list of possible reassembly rules for this key. 
					#
					if (defined $use_memory and $#{ $self->{reasmblist_for_memory}->{$reasmbkey} } >= 0) {

						# If this transform function was invoked with the memory flag, 
						# and there are in fact reassembly rules which are appropriate
						# for pulling out of memory, then include them.  
						@these_reasmbs = @{ $self->{reasmblist_for_memory}->{$reasmbkey} }

					} else {

						# Otherwise, just use the plain reassembly rules.
						# (This is what normally happens.)
						@these_reasmbs = @{ $self->{reasmblist}->{$reasmbkey} }
					}

					# Pick out a reassembly rule at random. 
					$reasmb = $these_reasmbs[ int &{$self->{myrand}}( scalar @these_reasmbs ) ];

					$self->debug_text($self->debug_text . sprintf "\t\t-->  $reasmb\n");

					# If the reassembly rule we picked contains the word "goto",
					# then we start over with a new keyword.  Set $keyword to equal
					# that word, and start the whole loop over. 
					if ($reasmb =~ m/^goto\s(\w*).*/i) {
						$self->debug_text($self->debug_text . sprintf "\$1 = $1\n");
						$goto = $keyword = $1;
						$rank = -2;
						redo KEYWORD;
					}

					if ($reasmb =~ m/lookupLastfm artist (.*)/i) {
						my $inputartist = $decomp_matches[2];
						$inputartist =~ s/^\s*//;
						$inputartist =~ s/\s*$//;
						$rank = 20;
						my @artists = getSimilarArtists($inputartist,2);
						my $resultcount=@artists;
						if ($resultcount < 1) {
							$reasmb = "**bzzt.*sorry I. do not *know anyone like $inputartist";
						} else {
							#print "CMD is $cmd\n";
							$reasmb = "*bzzt* Do you know $artists[0] or $artists[1]?";
						}
						
					}

					# Otherwise, using the matches to wildcards which we stored above,
					# insert words from the input string back into the reassembly rule. 
					# [THANKS to Gidon Wise for submitting a bugfix here]
					for ($i=1; $i <= $#decomp_matches; $i++) {
						$decomp_matches[$i] = $self->postprocess( $decomp_matches[$i] );
						$decomp_matches[$i] =~ s/([,;?!]|\.*)$//;
						$reasmb =~ s/\($i\)/$decomp_matches[$i]/g;
					}

					# Move on to the next keyword.  If no other keywords match,
					# then we'll end up actually using the $reasmb string 
					# we just generated above.
					next KEYWORD ;

				}  # End if ($string_part =~ /$this_decomp/i) 

				$self->debug_text($self->debug_text . sprintf "\n");

			} # End DECOMP: foreach $decomp (@{ $self->{decomplist}->{$keyword} }) 

		} # End if ( ($string_part =~ /\b$keyword\b/i or $keyword eq $goto) 

	} # End KEYWORD: foreach $keyword (keys %{ $self->{decomplist})
	
	} # End STRING_PARTS: foreach $string_part (@string_parts) {

=head2 How memory is used

In the script, some reassembly rules are special.  They are marked with
the keyword "reasm_for_memory", rather than just "reasm".  
Eliza "remembers" any comment when it matches a docomposition rule 
for which there are any reassembly rules for memory. 
An Eliza object remembers up to C<$max_memory_size> (default: 5) 
user input strings.  

If, during a subsequent run, the transform() method fails to find any 
appropriate decomposition rule for a user's comment, and if there are 
any comments inside the memory array, then Eliza may elect to ignore 
the most recent comment and instead pull out one of the strings from memory.  
In this case, the transform method is called recursively with the memory flag. 

Honestly, I am not sure exactly how this memory functionality
was implemented in the original Eliza program.  Hopefully
this implementation is not too far from Weizenbaum's. 

If you don't want to use the memory functionality at all,
then you can disable it:

	$mybot->memory_on(0);

You can also achieve the same effect by making sure
that the script data does not contain any reassembly rules 
marked with the keyword "reasm_for_memory".  The default
script data only has 4 such items.  

=cut

	if ($reasmb eq "") {

		# If all else fails, call this method recursively 
		# and make sure that it has something to parse. 
		# Use a string from memory if anything is available. 
		#
		# $self-likelihood_of_using_memory should be some number
		# between 1 and 0;  it defaults to 1. 
		#
		if (
			$#{ $self->memory } >= 0 
			and 
			&{$self->{myrand}}(1) >= 1 - $self->likelihood_of_using_memory
		) {

			$reasmb =  $self->transform( shift @{ $self->memory }, "use memory" );

		} else {
			$reasmb =  $self->transform("xnone");
		}

	} elsif ($self->memory_on) {   

		# If memory is switched on, then we handle memory. 

		# Now that we have successfully transformed this string, 
		# push it onto the end of the memory stack... unless, of course,
		# that's where we got it from in the first place, or if the rank
		# is not the kind we remember.
		#
		if (
				$#{ $self->{reasmblist_for_memory}->{$reasmbkey} } >= 0
				and
				not defined $use_memory
		) {

			push  @{ $self->memory },$string ;
		}

		# Shift out the least-recent item from the bottom 
		# of the memory stack if the stack exceeds the max size. 
		shift @{ $self->memory } if $#{ $self->memory } >= $self->max_memory_size;

		$self->debug_text($self->debug_text 
			. sprintf("\t%d item(s) in memory.\n", $#{ $self->memory } + 1 ) ) ;

	} # End if ($reasmb eq "")

	$reasmb =~ tr/ / /s;       # Eliminate any duplicate space characters. 
	$reasmb =~ s/[ ][?]$/?/;   # Eliminate any spaces before the question mark. 

	# Save the return string so that forgetful calling programs
	# can ask the bot what the last reply was. 
	$self->transform_text($reasmb);

	return "$reasmb";
}


####################################################################
# --- parse_script_data ---

=head2 parse_script_data()

    $self->parse_script_data;
    $self->parse_script_data( $script_file );

parse_script_data() is invoked from the _initialize() method,
which is called from the new() function.  However, you can also
call this method at any time against an already-instantiated 
Eliza instance.  In that case, the new script data is I<added>
to the old script data.  The old script data is not deleted. 

You can pass a parameter to this function, which is the name of the
script file, and it will read in and parse that file.  
If you do not pass any parameter to this method, then
it will read the data embedded at the end of the module as its
default script data.  

If you pass the name of a script file to parse_script_data(), 
and that file is not available for reading, then the module dies.  


=head1 Format of the script file

This module includes a default script file within itself, 
so it is not necessary to explicitly specify a script file 
when instantiating an Eliza object.  

Each line in the script file can specify a key,
a decomposition rule, or a reassembly rule.

  key: remember 5
    decomp: * i remember *
      reasmb: Do you often think of (2) ?
      reasmb: Does thinking of (2) bring anything else to mind ?
    decomp: * do you remember *
      reasmb: Did you think I would forget (2) ?
      reasmb: What about (2) ?
      reasmb: goto what
  pre: equivalent alike
  synon: belief feel think believe wish

The number after the key specifies the rank.
If a user's input contains the keyword, then
the transform() function will try to match
one of the decomposition rules for that keyword.
If one matches, then it will select one of
the reassembly rules at random.  The number
(2) here means "use whatever set of words
matched the second asterisk in the decomposition
rule." 

If you specify a list of synonyms for a word,
the you should use a "@" when you use that
word in a decomposition rule:

  decomp: * i @belief i *
    reasmb: Do you really think so ?
    reasmb: But you are not sure you (3).

Otherwise, the script will never check to see
if there are any synonyms for that keyword. 

Reassembly rules should be marked with I<reasm_for_memory>
rather than I<reasmb> when it is appropriate for use
when a user's comment has been extracted from memory. 

  key: my 2
    decomp: * my *
      reasm_for_memory: Let's discuss further why your (2).
      reasm_for_memory: Earlier you said your (2).
      reasm_for_memory: But your (2).
      reasm_for_memory: Does that have anything to do with the fact that your (2) ?

=head1 How the script file is parsed

Each line in the script file contains an "entrytype"
(key, decomp, synon) and an "entry", separated by
a colon.  In turn, each "entry" can itself be 
composed of a "key" and a "value", separated by
a space.  The parse_script_data() function
parses each line out, and splits the "entry" and
"entrytype" portion of each line into two variables,
C<$entry> and C<$entrytype>. 

Next, it uses the string C<$entrytype> to determine 
what sort of stuff to expect in the C<$entry> variable,  
if anything, and parses it accordingly.  In some cases,
there is no second level of key-value pair, so the function
does not even bother to isolate or create C<$key> and C<$value>. 

C<$key> is always a single word.  C<$value> can be null, 
or one single word, or a string composed of several words, 
or an array of words.  

Based on all these entries and keys and values,
the function creates two giant hashes:
C<%decomplist>, which holds the decomposition rules for
each keyword, and C<%reasmblist>, which holds the 
reassembly phrases for each decomposition rule. 
It also creates C<%keyranks>, which holds the ranks for
each key.  

Six other arrays are created: C<%reasm_for_memory, %pre, %post, 
%synon, @initial,> and C<@final>. 

=cut

sub parse_script_data {

	my ($self,$scriptfile) = @_;
	my @scriptlines;

	if ($scriptfile) {

		# If we have an external script file, open it 
		# and read it in (the whole thing, all at once). 
		open  (SCRIPTFILE, "<$scriptfile") 
			or die "Could not read from file $scriptfile : $!\n";
		@scriptlines = <SCRIPTFILE>; # read in script data 
		$self->scriptfile($scriptfile);
		close (SCRIPTFILE);

	} else {

		# Otherwise, read in the data from the bottom 
		# of this file.  This data might be read several
		# times, so we save the offset pointer and
		# reset it when we're done.
		my $where= tell(DATA);
		@scriptlines = <DATA>;  # read in script data 
		seek(DATA, $where, 0);
		$self->scriptfile('');
	}

	my ($entrytype, $entry, $key, $value) ;
	my $thiskey    = ""; 
	my $thisdecomp = "";

	############################################################
	# Examine each line of script data.  
	for (@scriptlines) { 

		# Skip comments and lines with only whitespace.
		next if (/^\s*#/ || /^\s*$/);  

		# Split entrytype and entry, using a colon as the delimiter.
		($entrytype, $entry) = $_ =~ m/^\s*(\S*)\s*:\s*(.*)\s*$/;

		# Case loop, based on the entrytype.
		for ($entrytype) {   

			/quit/		and do { push @{ $self->{quit}    }, $entry; last; };
			/initial/	and do { push @{ $self->{initial} }, $entry; last; };
			/final/		and do { push @{ $self->{final}   }, $entry; last; };

			/decomp/	and do { 
						die "$0: error parsing script:  decomposition rule with no keyword.\n" 
							if $thiskey eq "";
						$thisdecomp = join($;,$thiskey,$entry);
						push @{ $self->{decomplist}->{$thiskey} }, $entry ; 
						last; 
					};

			/reasmb/	and do { 
						die "$0: error parsing script:  reassembly rule with no decomposition rule.\n" 
							if $thisdecomp eq "";
						push @{ $self->{reasmblist}->{$thisdecomp} }, $entry ;  
						last; 
					};

			/reasm_for_memory/	and do { 
						die "$0: error parsing script:  reassembly rule with no decomposition rule.\n" 
							if $thisdecomp eq "";
						push @{ $self->{reasmblist_for_memory}->{$thisdecomp} }, $entry ;  
						last; 
					};

			# The entrytypes below actually expect to see a key and value
			# pair in the entry, so we split them out.  The first word, 
			# separated by a space, is the key, and everything else is 
			# an array of values.

			($key,$value) = $entry =~ m/^\s*(\S*)\s*(.*)/;

			/pre/		and do { $self->{pre}->{$key}   = $value; last; };
			/post/		and do { $self->{post}->{$key}  = $value; last; };

			# synon expects an array, so we split $value into an array, using " " as delimiter.  
			/synon/		and do { $self->{synon}->{$key} = [ split /\ /, $value ]; last; };

			/key/		and do { 
						$thiskey = $key; 
						$thisdecomp = "";
						$self->{keyranks}->{$thiskey} = $value ; 
						last;
					};
	
		}  # End for ($entrytype) (case loop) 

	}  # End for (@scriptlines)

}  # End of method parse_script_data

###################################################################
####### LASTFM WS SPECFICZ

sub getSimilarArtists {

	my $method = 'artist.getSimilar';
	my $artist = shift;
	my $limit = shift;
	$artist =~ s/^\s*//;
	$artist =~ s/\s*$//;
	# these are ordered and used to sign the WS auth key in the next step
	my %rsubparams;
	$rsubparams{ 'api_key' } = $api_key;
	$rsubparams{ 'method' } = $method;
	$rsubparams{ 'limit' } = $limit;

	# construct encoded api sig and then full url
	my $enc_api_sig = sigConstruct0r(\%rsubparams);
	my $req = "$rooturl" . "?" . "method=$method&artist=$artist&api_key=$api_key&limit=$limit";

	my $client = REST::Client->new();
	$client->GET("$req");
	my $recz = $client->responseContent();

	my $xz = XML::Simple->new;
	my $xrecz = $xz->XMLin($recz);

	my @simartists;
	#foreach my $rec (keys %{$xrecz->{recommendations}->{artist}}) {
	foreach my $key (@{$xrecz->{similarartists}->{artist}}) {
		next if uc($key) eq uc($artist);
		my $simart = $key->{name};
		push (@simartists,$simart);
	}
	return @simartists;
}

sub sigConstruct0r {
       my $api_sig;
       my %subparams = %{$_[0]};
       foreach my $key (sort (keys(%subparams))) {
               $api_sig .= "$key" . "$subparams{$key}";
       }
       #print "APISIG = $api_sig\n";
       my $enc_api_sig = md5_hex("$api_sig");
       return $enc_api_sig;
}

# Eliminate some pesky warnings.
#
sub DESTROY {}

# ---{ E N D   M E T H O D S }----------------------------------
####################################################################

1;  	# Return a true value.  

