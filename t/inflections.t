# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..1189\n"; }
END {print "not ok 1\n" unless $loaded;}
use Lingua::EN::Inflect qw( :ALL );
$loaded = 1;
print "ok 1\n";

my $count = 2;
sub ok($;$)
{
	my $ok = $_[0];
	print "not " unless $ok;
	print "ok $count";
	# print "\t($_[1])" if $_[1];
	print "\n";
	$count++;
	return $ok;
}

######################### End of black magic.

sub test_eq($$)
{
	PL_eq($_[0],$_[1])    ||
	PL_N_eq($_[0],$_[1])  ||
	PL_V_eq($_[0],$_[1])  ||
	PL_ADJ_eq($_[0],$_[1]);
}

foreach (<DATA>)
{
	if (/^\s*(.*?)\s*->\s*(.*?)\s*(?:\|\s*(.*?)\s*)?(#\s*(.*))?$/)
	{
		$singular = $1;
		$plural   = $2;
		$altplural = $3 || "";
		$comment  = $5 || '';
		$is_nv    = ($comment =~ /verb/i) ? '_V'
			  : ($comment =~ /noun/i) ? '_N'
			  : '';
		$PL_V     = PL_V($singular);
		$PL_N     = PL_N($singular);
		$PL       = PL($singular);
		$PL_val   = ($is_nv eq '_V') ? $PL_V
			  : ($is_nv eq '_N') ? $PL_N
			  : $PL;

		my $altclass = $classical && $altplural;

		# print "$singular -> $PL_val : ";

		ok (
			(!$altclass && $plural eq $PL_val || $altclass && $altplural eq $PL_val) 
		    &&
			( test_eq($singular,$plural) && test_eq($plural,$singular) )
		    &&
		 	( !$altplural || 
				( test_eq($singular,$altplural) &&
				  test_eq($altplural,$singular) &&
			      	  test_eq($plural,$altplural)   &&
				  test_eq($altplural,$plural))
			)
		   , $singular
		   )
	}


	elsif (/^\s+(an?)\s+(.*?)\s*$/)
	{
		$article = $1;
		$word    = $2;
		$Aword   = A($word);

		ok ("$article $word" eq $Aword);
	}
}

def_noun "kin" => "kine";
def_noun '(.*)x' => '$1xen';

def_verb 'foobar'  => 'feebar',
         'foobar'  => 'feebar',
         'foobars' => 'feebar';

def_adj  'red' => 'red|gules';

ok ( NO("kin",0) eq "no kine" );
ok ( NO("kin",1) eq "1 kin" );
ok ( NO("kin",2) eq "2 kine" );

ok ( NO("regex",0) eq "no regexen" );

ok ( PL("foobar",2) eq "feebar" );
ok ( PL("foobars",2) eq "feebar" );

classical 0;
ok ( PL("red",0) eq "red" );
ok ( PL("red",1) eq "red" );
ok ( PL("red",2) eq "red" );
classical 1;
ok ( PL("red",0) eq "red" );
ok ( PL("red",1) eq "red" );
ok ( PL("red",2) eq "gules" );

ok ( ORD(0) eq "0th" );
ok ( ORD(1) eq "1st" );
ok ( ORD(2) eq "2nd" );
ok ( ORD(3) eq "3rd" );
ok ( ORD(4) eq "4th" );
ok ( ORD(5) eq "5th" );
ok ( ORD(6) eq "6th" );
ok ( ORD(7) eq "7th" );
ok ( ORD(8) eq "8th" );
ok ( ORD(9) eq "9th" );
ok ( ORD(10) eq "10th" );
ok ( ORD(11) eq "11th" );
ok ( ORD(12) eq "12th" );
ok ( ORD(13) eq "13th" );
ok ( ORD(14) eq "14th" );
ok ( ORD(15) eq "15th" );
ok ( ORD(16) eq "16th" );
ok ( ORD(17) eq "17th" );
ok ( ORD(18) eq "18th" );
ok ( ORD(19) eq "19th" );
ok ( ORD(20) eq "20th" );
ok ( ORD(21) eq "21st" );
ok ( ORD(22) eq "22nd" );
ok ( ORD(23) eq "23rd" );
ok ( ORD(24) eq "24th" );
ok ( ORD(100) eq "100th" );
ok ( ORD(101) eq "101st" );
ok ( ORD(102) eq "102nd" );
ok ( ORD(103) eq "103rd" );
ok ( ORD(104) eq "104th" );

ok ( PART_PRES("sees") eq "seeing" );
ok ( PART_PRES("eats") eq "eating" );
ok ( PART_PRES("bats") eq "batting" );
ok ( PART_PRES("hates") eq "hating" );
ok ( PART_PRES("spies") eq "spying" );

for my $i (0..$#nw)
{
	ok ( NUMWORDS($nw[$i][0])           eq $nw[$i][1] );
	ok ( NUMWORDS($nw[$i][0], group=>1) eq $nw[$i][2] );
	ok ( NUMWORDS($nw[$i][0], group=>2) eq $nw[$i][3] );
	ok ( NUMWORDS($nw[$i][0], group=>3) eq $nw[$i][4] );
	
}

BEGIN
{
	@nw = 
	(
	[
		"0",
		"zero",
		"zero",
		"zero",
		"zero",
	],[
		"1",
		"one",
		"one",
		"one",
		"one",
	],[
		"2",
		"two",
		"two",
		"two",
		"two",
	],[
		"3",
		"three",
		"three",
		"three",
		"three",
	],[
		"4",
		"four",
		"four",
		"four",
		"four",
	],[
		"5",
		"five",
		"five",
		"five",
		"five",
	],[
		"6",
		"six",
		"six",
		"six",
		"six",
	],[
		"7",
		"seven",
		"seven",
		"seven",
		"seven",
	],[
		"8",
		"eight",
		"eight",
		"eight",
		"eight",
	],[
		"9",
		"nine",
		"nine",
		"nine",
		"nine",
	],[
		"10",
		"ten",
		"one, zero",
		"ten",
		"ten",
	],[
		"11",
		"eleven",
		"one, one",
		"eleven",
		"eleven",
	],[
		"12",
		"twelve",
		"one, two",
		"twelve",
		"twelve",
	],[
		"13",
		"thirteen",
		"one, three",
		"thirteen",
		"thirteen",
	],[
		"14",
		"fourteen",
		"one, four",
		"fourteen",
		"fourteen",
	],[
		"15",
		"fifteen",
		"one, five",
		"fifteen",
		"fifteen",
	],[
		"16",
		"sixteen",
		"one, six",
		"sixteen",
		"sixteen",
	],[
		"17",
		"seventeen",
		"one, seven",
		"seventeen",
		"seventeen",
	],[
		"18",
		"eighteen",
		"one, eight",
		"eighteen",
		"eighteen",
	],[
		"19",
		"nineteen",
		"one, nine",
		"nineteen",
		"nineteen",
	],[
		"20",
		"twenty",
		"two, zero",
		"twenty",
		"twenty",
	],[
		"21",
		"twenty-one",
		"two, one",
		"twenty-one",
		"twenty-one",
	],[
		"29",
		"twenty-nine",
		"two, nine",
		"twenty-nine",
		"twenty-nine",
	],[
		"99",
		"ninety-nine",
		"nine, nine",
		"ninety-nine",
		"ninety-nine",
	],[
		"100",
		"one hundred",
		"one, zero, zero",
		"ten, zero",
		"one zero zero",
	],[
		"101",
		"one hundred and one",
		"one, zero, one",
		"ten, one",
		"one zero one",
	],[
		"110",
		"one hundred and ten",
		"one, one, zero",
		"eleven, zero",
		"one ten",
	],[
		"111",
		"one hundred and eleven",
		"one, one, one",
		"eleven, one",
		"one eleven",
	],[
		"900",
		"nine hundred",
		"nine, zero, zero",
		"ninety, zero",
		"nine zero zero",
	],[
		"999",
		"nine hundred and ninety-nine",
		"nine, nine, nine",
		"ninety-nine, nine",
		"nine ninety-nine",
	],[
		"1000",
		"one thousand",
		"one, zero, zero, zero",
		"ten, zero zero",
		"one zero zero, zero",
	],[
		"1001",
		"one thousand and one",
		"one, zero, zero, one",
		"ten, zero one",
		"one zero zero, one",
	],[
		"1010",
		"one thousand and ten",
		"one, zero, one, zero",
		"ten, ten",
		"one zero one, zero",
	],[
		"1100",
		"one thousand, one hundred",
		"one, one, zero, zero",
		"eleven, zero zero",
		"one ten, zero",
	],[
		"2000",
		"two thousand",
		"two, zero, zero, zero",
		"twenty, zero zero",
		"two zero zero, zero",
	],[
		"10000",
		"ten thousand",
		"one, zero, zero, zero, zero",
		"ten, zero zero, zero",
		"one zero zero, zero zero",
	],[
		"100000",
		"one hundred thousand",
		"one, zero, zero, zero, zero, zero",
		"ten, zero zero, zero zero",
		"one zero zero, zero zero zero",
	],[
		"100001",
		"one hundred thousand and one",
		"one, zero, zero, zero, zero, one",
		"ten, zero zero, zero one",
		"one zero zero, zero zero one",
	],[
		"123456",
		"one hundred and twenty-three thousand, four hundred and fifty-six",
		"one, two, three, four, five, six",
		"twelve, thirty-four, fifty-six",
		"one twenty-three, four fifty-six",
	],[
		"0123456",
		"one hundred and twenty-three thousand, four hundred and fifty-six",
		"zero, one, two, three, four, five, six",
		"zero one, twenty-three, forty-five, six",
		"zero twelve, three forty-five, six",
	],[
		"1234567",
		"one million, two hundred and thirty-four thousand, five hundred and sixty-seven",
		"one, two, three, four, five, six, seven",
		"twelve, thirty-four, fifty-six, seven",
		"one twenty-three, four fifty-six, seven",
	],[
		"12345678",
		"twelve million, three hundred and forty-five thousand, six hundred and seventy-eight",
		"one, two, three, four, five, six, seven, eight",
		"twelve, thirty-four, fifty-six, seventy-eight",
		"one twenty-three, four fifty-six, seventy-eight",
	],[
		"12_345_678",
		"twelve million, three hundred and forty-five thousand, six hundred and seventy-eight",
		"one, two, three, four, five, six, seven, eight",
		"twelve, thirty-four, fifty-six, seventy-eight",
		"one twenty-three, four fifty-six, seventy-eight",
	],[
		"1234,5678",
		"twelve million, three hundred and forty-five thousand, six hundred and seventy-eight",
		"one, two, three, four, five, six, seven, eight",
		"twelve, thirty-four, fifty-six, seventy-eight",
		"one twenty-three, four fifty-six, seventy-eight",
	],[
		"1234567890",
		"one billion, two hundred and thirty-four million, five hundred and sixty-seven thousand, eight hundred and ninety",
		"one, two, three, four, five, six, seven, eight, nine, zero",
		"twelve, thirty-four, fifty-six, seventy-eight, ninety",
		"one twenty-three, four fifty-six, seven eighty-nine, zero",
	],[
		"123456789012345",
		"one hundred and twenty-three trillion, four hundred and fifty-six billion, seven hundred and eighty-nine million, twelve thousand, three hundred and forty-five",
		"one, two, three, four, five, six, seven, eight, nine, zero, one, two, three, four, five",
		"twelve, thirty-four, fifty-six, seventy-eight, ninety, twelve, thirty-four, five",
		"one twenty-three, four fifty-six, seven eighty-nine, zero twelve, three forty-five",
	],[
		"12345678901234567890",
		"twelve quintillion, three hundred and forty-five quadrillion, six hundred and seventy-eight trillion, nine hundred and one billion, two hundred and thirty-four million, five hundred and sixty-seven thousand, eight hundred and ninety",
		"one, two, three, four, five, six, seven, eight, nine, zero, one, two, three, four, five, six, seven, eight, nine, zero",
		"twelve, thirty-four, fifty-six, seventy-eight, ninety, twelve, thirty-four, fifty-six, seventy-eight, ninety",
		"one twenty-three, four fifty-six, seven eighty-nine, zero twelve, three forty-five, six seventy-eight, ninety",
	],[
		"0.987654",
		"zero point nine eight seven six five four",
		"zero, point, nine, eight, seven, six, five, four",
		"zero, point, ninety-eight, seventy-six, fifty-four",
		"zero, point, nine eighty-seven, six fifty-four",
	],[
		".987654",
		"point nine eight seven six five four",
		"point, nine, eight, seven, six, five, four",
		"point, ninety-eight, seventy-six, fifty-four",
		"point, nine eighty-seven, six fifty-four",
	],[
		"9.87654",
		"nine point eight seven six five four",
		"nine, point, eight, seven, six, five, four",
		"nine, point, eighty-seven, sixty-five, four",
		"nine, point, eight seventy-six, fifty-four",
	],[
		"98.7654",
		"ninety-eight point seven six five four",
		"nine, eight, point, seven, six, five, four",
		"ninety-eight, point, seventy-six, fifty-four",
		"ninety-eight, point, seven sixty-five, four",
	],[
		"987.654",
		"nine hundred and eighty-seven point six five four",
		"nine, eight, seven, point, six, five, four",
		"ninety-eight, seven, point, sixty-five, four",
		"nine eighty-seven, point, six fifty-four",
	],[
		"9876.54",
		"nine thousand, eight hundred and seventy-six point five four",
		"nine, eight, seven, six, point, five, four",
		"ninety-eight, seventy-six, point, fifty-four",
		"nine eighty-seven, six, point, fifty-four",
	],[
		"98765.4",
		"ninety-eight thousand, seven hundred and sixty-five point four",
		"nine, eight, seven, six, five, point, four",
		"ninety-eight, seventy-six, five, point, four",
		"nine eighty-seven, sixty-five, point, four",
	],[
		"101.202.303",
		"one hundred and one point two zero two three zero three",
		"one, zero, one, point, two, zero, two, point, three, zero, three",
		"ten, one, point, twenty, two, point, thirty, three",
		"one zero one, point, two zero two, point, three zero three",
	]
	);
}

__DATA__
                    a  ->  some                         # INDEFINITE ARTICLE
                    a  ->  as                           # NOUN FORM
       A.C.R.O.N.Y.M.  ->  A.C.R.O.N.Y.M.s
             abscissa  ->  abscissas|abscissae
             Achinese  ->  Achinese
            acropolis  ->  acropolises
                adieu  ->  adieus|adieux
     adjutant general  ->  adjutant generals
                aegis  ->  aegises
             afflatus  ->  afflatuses
               afreet  ->  afreets|afreeti
                afrit  ->  afrits|afriti
              agendum  ->  agenda
         aide-de-camp  ->  aides-de-camp
             Alabaman  ->  Alabamans
               albino  ->  albinos
                album  ->  albums
             Alfurese  ->  Alfurese
                 alga  ->  algae
                alias  ->  aliases
                 alto  ->  altos|alti
               alumna  ->  alumnae
              alumnus  ->  alumni
             alveolus  ->  alveoli
                   am  ->  are
             am going  ->  are going
  ambassador-at-large  ->  ambassadors-at-large
            Amboinese  ->  Amboinese
          Americanese  ->  Americanese
               amoeba  ->  amoebas|amoebae
              Amoyese  ->  Amoyese
                   an  ->  some                         # INDEFINITE ARTICLE
             analysis  ->  analyses
             anathema  ->  anathemas|anathemata
           Andamanese  ->  Andamanese
             Angolese  ->  Angolese
             Annamese  ->  Annamese
              antenna  ->  antennas|antennae
                 anus  ->  anuses
                 apex  ->  apexes|apices
               apex's  ->  apexes'|apices'              # POSSESSIVE FORM
             aphelion  ->  aphelia
            apparatus  ->  apparatuses|apparatus
             appendix  ->  appendixes|appendices
                apple  ->  apples
             aquarium  ->  aquariums|aquaria
            Aragonese  ->  Aragonese
            Arakanese  ->  Arakanese
          archipelago  ->  archipelagos
                  are  ->  are
             are made  ->  are made
            armadillo  ->  armadillos
             arpeggio  ->  arpeggios
            arthritis  ->  arthritises
             asbestos  ->  asbestoses
            asparagus  ->  asparaguses
                  ass  ->  asses
             Assamese  ->  Assamese
               asylum  ->  asylums
            asyndeton  ->  asyndeta
                at it  ->  at them                      # ACCUSATIVE
                  ate  ->  ate
                atlas  ->  atlases
     attorney general  ->  attorneys general
   attorney of record  ->  attorneys of record
               aurora  ->  auroras|aurorae
             aviatrix  ->  aviatrixes|aviatrices
           aviatrix's  ->  aviatrixes'|aviatrices'
           Avignonese  ->  Avignonese
                  axe  ->  axes
                 axis  ->  axes
        Azerbaijanese  ->  Azerbaijanese
             bacillus  ->  bacilli
            bacterium  ->  bacteria
              Bahaman  ->  Bahamans
             Balinese  ->  Balinese
               bamboo  ->  bamboos
                banjo  ->  banjoes
                 bass  ->  basses                       # INSTRUMENT, NOT FISH
                basso  ->  bassos|bassi
               bathos  ->  bathoses
                 beau  ->  beaus|beaux
                 beef  ->  beefs|beeves
           beneath it  ->  beneath them                 # ACCUSATIVE
            Bengalese  ->  Bengalese
                 bent  ->  bent                         # VERB FORM
                 bent  ->  bents                        # NOUN FORM
              Bernese  ->  Bernese
            Bhutanese  ->  Bhutanese
                 bias  ->  biases
               biceps  ->  biceps
                bison  ->  bisons|bison
            Bolognese  ->  Bolognese
                bonus  ->  bonuses
             Borghese  ->  Borghese
                 boss  ->  bosses
            Bostonese  ->  Bostonese
                  box  ->  boxes
                  boy  ->  boys
                bravo  ->  bravoes
                bream  ->  bream
             breeches  ->  breeches
          bride-to-be  ->  brides-to-be
             britches  ->  britches
           bronchitis  ->  bronchitises
             bronchus  ->  bronchi
              brother  ->  brothers|brethren
            brother's  ->  brothers'|brethren's
              buffalo  ->  buffaloes|buffalo
             Buginese  ->  Buginese
                 buoy  ->  buoys
               bureau  ->  bureaus|bureaux
               Burman  ->  Burmans
              Burmese  ->  Burmese
             bursitis  ->  bursitises
                  bus  ->  buses
                 buzz  ->  buzzes
               buzzes  ->  buzz                         # VERB FORM
                by it  ->  by them                      # ACCUSATIVE
               caddis  ->  caddises
                 cake  ->  cakes
            Calabrese  ->  Calabrese
                 calf  ->  calves
               callus  ->  calluses
          Camaldolese  ->  Camaldolese
                cameo  ->  cameos
               campus  ->  campuses
                  can  ->  cans                         # NOUN FORM
                  can  ->  can                          # VERB FORM (all pers.)
          candelabrum  ->  candelabra
             cannabis  ->  cannabises
                canto  ->  cantos
            Cantonese  ->  Cantonese
               cantus  ->  cantus
               canvas  ->  canvases
              CAPITAL  ->  CAPITALS
            carcinoma  ->  carcinomas|carcinomata
                 care  ->  cares
                cargo  ->  cargoes
            Carlylese  ->  Carlylese
                 carp  ->  carp
            Cassinese  ->  Cassinese
                  cat  ->  cats
              catfish  ->  catfish
             Celanese  ->  Celanese
            Ceylonese  ->  Ceylonese
             chairman  ->  chairmen
              chamois  ->  chamois
                chaos  ->  chaoses
              chapeau  ->  chapeaus|chapeaux
             charisma  ->  charismas|charismata
               chases  ->  chase
              chassis  ->  chassis
              chateau  ->  chateaus|chateaux
               cherub  ->  cherubs|cherubim
           chickenpox  ->  chickenpox
                chief  ->  chiefs
                child  ->  children
              Chinese  ->  Chinese
               chorus  ->  choruses
               church  ->  churches
             cicatrix  ->  cicatrixes|cicatrices
               circus  ->  circuses
                class  ->  classes
              classes  ->  class                        # VERB FORM
             clippers  ->  clippers
             clitoris  ->  clitorises|clitorides
                  cod  ->  cod
                codex  ->  codices
               coitus  ->  coitus
             commando  ->  commandos
           compendium  ->  compendiums|compendia
             Congoese  ->  Congoese
            Congolese  ->  Congolese
           conspectus  ->  conspectuses
            contralto  ->  contraltos|contralti
          contretemps  ->  contretemps
            conundrum  ->  conundrums
                corps  ->  corps
               corpus  ->  corpuses|corpora
               cortex  ->  cortexes|cortices
               cosmos  ->  cosmoses
        court martial  ->  courts martial
                  cow  ->  cows|kine
              cranium  ->  craniums|crania
            crescendo  ->  crescendos
            criterion  ->  criteria
           curriculum  ->  curriculums|curricula
                 dais  ->  daises
           data point  ->  data points
                datum  ->  data
               debris  ->  debris
              decorum  ->  decorums
                 deer  ->  deer
           delphinium  ->  delphiniums
          desideratum  ->  desiderata
             diabetes  ->  diabetes
               dictum  ->  dictums|dicta
                  did  ->  did
             did need  ->  did need
            digitalis  ->  digitalises
                dingo  ->  dingoes
              diploma  ->  diplomas|diplomata
               discus  ->  discuses
                 dish  ->  dishes
                ditto  ->  dittos
                djinn  ->  djinn
                 does  ->  do
                  dog  ->  dogs
                dogma  ->  dogmas|dogmata
           dominatrix  ->  dominatrixes|dominatrices
               domino  ->  dominoes
            Dongolese  ->  Dongolese
                drama  ->  dramas|dramata
                 drum  ->  drums
                dwarf  ->  dwarves
               dynamo  ->  dynamos
                edema  ->  edemas|edemata
                eland  ->  elands|eland
                  elf  ->  elves
                  elk  ->  elks|elk
               embryo  ->  embryos
             emporium  ->  emporiums|emporia
         encephalitis  ->  encephalitises
             enconium  ->  enconiums|enconia
                enema  ->  enemas|enemata
               enigma  ->  enigmas|enigmata
            ephemeris  ->  ephemerides
            epidermis  ->  epidermises
              erratum  ->  errata
                ethos  ->  ethoses
           eucalyptus  ->  eucalyptuses
             extremum  ->  extrema
                 eyas  ->  eyases
             factotum  ->  factotums
              Faroese  ->  Faroese
                fauna  ->  faunas|faunae
                  fax  ->  faxes
            Ferrarese  ->  Ferrarese
                ferry  ->  ferries
                fetus  ->  fetuses
               fiance  ->  fiances
              fiancee  ->  fiancees
               fiasco  ->  fiascos
                 fish  ->  fish
                 fizz  ->  fizzes
             flamingo  ->  flamingoes
                flora  ->  floras|florae
             flounder  ->  flounder
                focus  ->  focuses|foci
               foetus  ->  foetuses
                folio  ->  folios
           Foochowese  ->  Foochowese
                 foot  ->  feet
               foot's  ->  feet's                       # POSSESSIVE FORM
              foramen  ->  foramens|foramina
              formula  ->  formulas|formulae
                forum  ->  forums
               fought  ->  fought
                  fox  ->  foxes
             from him  ->  from them
              from it  ->  from them                    # ACCUSATIVE
               fungus  ->  funguses|fungi
             Gabunese  ->  Gabunese
              gallows  ->  gallows
             ganglion  ->  ganglions|ganglia
                  gas  ->  gases
               gateau  ->  gateaus|gateaux
                 gave  ->  gave
        generalissimo  ->  generalissimos
             Genevese  ->  Genevese
                genie  ->  genies|genii
               genius  ->  geniuses|genii
              Genoese  ->  Genoese
                genus  ->  genera
               German  ->  Germans
               ghetto  ->  ghettos
           Gilbertese  ->  Gilbertese
              glottis  ->  glottises
              Goanese  ->  Goanese
                goose  ->  geese
     Governor General  ->  Governors General
                  goy  ->  goys|goyim
             graffiti  ->  graffiti
             graffito  ->  graffiti
                guano  ->  guanos
            guardsman  ->  guardsmen
             Guianese  ->  Guianese
                gumma  ->  gummas|gummata
            gymnasium  ->  gymnasiums|gymnasia
                  had  ->  had
          had thought  ->  had thought
            Hainanese  ->  Hainanese
         handkerchief  ->  handkerchiefs
             Hararese  ->  Hararese
            Harlemese  ->  Harlemese
            harmonium  ->  harmoniums
                  has  ->  have
           has become  ->  have become
             has been  ->  have been
             has-been  ->  has-beens
             Havanese  ->  Havanese
                 have  ->  have
        have conceded  ->  have conceded
                   he  ->  they
         headquarters  ->  headquarters
            Heavenese  ->  Heavenese
                helix  ->  helices
            hepatitis  ->  hepatitises
                  her  ->  them                         # PRONOUN
                  her  ->  their                        # POSSESSIVE ADJ
                 hero  ->  heroes
               herpes  ->  herpes
                 hers  ->  theirs                       # POSSESSIVE NOUN
              herself  ->  themselves
               hiatus  ->  hiatuses|hiatus
            highlight  ->  highlights
              hijinks  ->  hijinks
                  him  ->  them
              himself  ->  themselves
         hippopotamus  ->  hippopotamuses|hippopotami
           Hiroshiman  ->  Hiroshimans
                  his  ->  their                        # POSSESSIVE ADJ
                  his  ->  theirs                       # POSSESSIVE NOUN
           honorarium  ->  honorariums|honoraria
                 hoof  ->  hoofs|hooves
           Hoosierese  ->  Hoosierese
         Hottentotese  ->  Hottentotese
                house  ->  houses
            housewife  ->  housewives
               hubris  ->  hubrises
                human  ->  humans
             Hunanese  ->  Hunanese
                hydra  ->  hydras|hydrae
           hyperbaton  ->  hyperbata
            hyperbola  ->  hyperbolas|hyperbolae
                    I  ->  we
                 ibis  ->  ibises
            ignoramus  ->  ignoramuses
              impetus  ->  impetuses|impetus
              incubus  ->  incubuses|incubi
                index  ->  indexes|indices
          Indochinese  ->  Indochinese
              inferno  ->  infernos
              innings  ->  innings
    Inspector General  ->  Inspectors General
          interregnum  ->  interregnums|interregna
                 iris  ->  irises|irides
                   is  ->  are
             is eaten  ->  are eaten
                   it  ->  they                         # NOMINATIVE
                  its  ->  their                        # POSSESSIVE FORM
               itself  ->  themselves
           jackanapes  ->  jackanapes
             Japanese  ->  Japanese
             Javanese  ->  Javanese
                Jerry  ->  Jerrys
                jerry  ->  jerries
                 jinx  ->  jinxes
               jinxes  ->  jinx                         # VERB FORM
           Johnsonese  ->  Johnsonese
                Jones  ->  Joneses
                jumbo  ->  jumbos
             Kanarese  ->  Kanarese
           Kiplingese  ->  Kiplingese
                knife  ->  knives                       # NOUN FORM
                knife  ->  knife                        # VERB FORM (1st/2nd pers.)
               knifes  ->  knife                        # VERB FORM (3rd pers.)
             Kongoese  ->  Kongoese
            Kongolese  ->  Kongolese
               lacuna  ->  lacunas|lacunae
      lady in waiting  ->  ladies in waiting
            Lapponese  ->  Lapponese
               larynx  ->  larynxes|larynges
                latex  ->  latexes|latices
                 leaf  ->  leaf                         # VERB FORM (1st/2nd pers.)
                 leaf  ->  leaves                       # NOUN FORM
                leafs  ->  leaf                         # VERB FORM (3rd pers.)
             Lebanese  ->  Lebanese
                lemma  ->  lemmas|lemmata
                 lens  ->  lenses
              Leonese  ->  Leonese
      lick of the cat  ->  licks of the cat
   Lieutenant General  ->  Lieutenant Generals
                 life  ->  lives
                Liman  ->  Limans
                lingo  ->  lingos
                 loaf  ->  loaves
                locus  ->  loci
            Londonese  ->  Londonese
           Lorrainese  ->  Lorrainese
             lothario  ->  lotharios
                louse  ->  lice
             Lucchese  ->  Lucchese
              lumbago  ->  lumbagos
                lumen  ->  lumens|lumina
              lustrum  ->  lustrums|lustra
               lyceum  ->  lyceums
             lymphoma  ->  lymphomas|lymphomata
                 lynx  ->  lynxes
              Lyonese  ->  Lyonese
               M.I.A.  ->  M.I.A.s
             Macanese  ->  Macanese
          Macassarese  ->  Macassarese
             mackerel  ->  mackerel
                 made  ->  made
             Madurese  ->  Madurese
                magma  ->  magmas|magmata
              magneto  ->  magnetos
        Major General  ->  Major Generals
           Malabarese  ->  Malabarese
              Maltese  ->  Maltese
                  man  ->  men
             mandamus  ->  mandamuses
            manifesto  ->  manifestos
               mantis  ->  mantises
              marquis  ->  marquises
                 Mary  ->  Marys
              maximum  ->  maximums|maxima
              measles  ->  measles
               medico  ->  medicos
               medium  ->  mediums|media
             medium's  ->  mediums'|media's
               medusa  ->  medusas|medusae
           memorandum  ->  memorandums|memoranda
             meniscus  ->  menisci
            Messinese  ->  Messinese
        metamorphosis  ->  metamorphoses
           metropolis  ->  metropolises
                 mews  ->  mews
               miasma  ->  miasmas|miasmata
             Milanese  ->  Milanese
               milieu  ->  milieus|milieux
            millenium  ->  milleniums|millenia
              minimum  ->  minimums|minima
                 minx  ->  minxes
                 miss  ->  miss                         # VERB FORM (1st/2nd pers.)
                 miss  ->  misses                       # NOUN FORM
               misses  ->  miss                         # VERB FORM (3rd pers.)
             mittamus  ->  mittamuses
             Modenese  ->  Modenese
             momentum  ->  momentums|momenta
                money  ->  monies
             mongoose  ->  mongooses
                moose  ->  mooses|moose
        mother-in-law  ->  mothers-in-law
                mouse  ->  mice
                mumps  ->  mumps
             Muranese  ->  Muranese
                murex  ->  murices
               museum  ->  museums
            mustachio  ->  mustachios
                   my  ->  our                          # POSSESSIVE FORM
               myself  ->  ourselves
               mythos  ->  mythoi
            Nakayaman  ->  Nakayamans
           Nankingese  ->  Nankingese
           nasturtium  ->  nasturtiums
            Navarrese  ->  Navarrese
               nebula  ->  nebulas|nebulae
             Nepalese  ->  Nepalese
             neuritis  ->  neuritises
             neurosis  ->  neuroses
                 news  ->  news
                nexus  ->  nexus
              Niasese  ->  Niasese
           Nicobarese  ->  Nicobarese
               nimbus  ->  nimbuses|nimbi
            Nipponese  ->  Nipponese
                   no  ->  noes
              nostrum  ->  nostrums
             noumenon  ->  noumena
                 nova  ->  novas|novae
            nucleolus  ->  nucleoluses|nucleoli
              nucleus  ->  nuclei
                  oaf  ->  oafs
               octavo  ->  octavos
              octopus  ->  octopuses|octopodes
               oedema  ->  oedemas|oedemata
            Oklahoman  ->  Oklahomans
              omnibus  ->  omnibuses
                on it  ->  on them                      # ACCUSATIVE
                 onus  ->  onuses
                opera  ->  operas
              optimum  ->  optimums|optima
                 opus  ->  opuses|opera
              organon  ->  organa
          ought to be  ->  ought to be                  # VERB (UNLIKE bride to be)
                 ovum  ->  ova
                   ox  ->  oxen
                 ox's  ->  oxen's                       # POSSESSIVE FORM
             oxymoron  ->  oxymorons|oxymora
              Panaman  ->  Panamans
             parabola  ->  parabolas|parabolae
              Parmese  ->  Parmese
               pathos  ->  pathoses
              pegasus  ->  pegasuses
            Pekingese  ->  Pekingese
               pelvis  ->  pelvises
             pendulum  ->  pendulums
                penis  ->  penises|penes
             penumbra  ->  penumbras|penumbrae
           perihelion  ->  perihelia
              persona  ->  personae
            petroleum  ->  petroleums
              phalanx  ->  phalanxes|phalanges
                  PhD  ->  PhDs
           phenomenon  ->  phenomena
             philtrum  ->  philtrums
                photo  ->  photos
               phylum  ->  phylums|phyla
                piano  ->  pianos|piani
          Piedmontese  ->  Piedmontese
               pincer  ->  pincers
              pincers  ->  pincers
            Pistoiese  ->  Pistoiese
              plateau  ->  plateaus|plateaux
                 play  ->  plays
               plexus  ->  plexuses|plexus
               pliers  ->  pliers
                plies  ->  ply                          # VERB FORM
                polis  ->  polises
             Polonese  ->  Polonese
             pontifex  ->  pontifexes|pontifices
          portmanteau  ->  portmanteaus|portmanteaux
           Portuguese  ->  Portuguese
               potato  ->  potatoes
                  pox  ->  pox
               pragma  ->  pragmas|pragmata
              premium  ->  premiums
          prima donna  ->  prima donnas|prime donne
                  pro  ->  pros
          proceedings  ->  proceedings
         prolegomenon  ->  prolegomena
                proof  ->  proofs
     proof of concept  ->  proofs of concept
          prosecutrix  ->  prosecutrixes|prosecutrices
           prospectus  ->  prospectuses|prospectus
            protozoan  ->  protozoans
            protozoon  ->  protozoa
                  put  ->  put
              quantum  ->  quantums|quanta
quartermaster general  ->  quartermasters general
               quarto  ->  quartos
               quorum  ->  quorums
               rabies  ->  rabies
               radius  ->  radiuses|radii
                radix  ->  radices
                rebus  ->  rebuses
             reindeer  ->  reindeer
                rhino  ->  rhinos
           rhinoceros  ->  rhinoceroses|rhinoceros
            Romagnese  ->  Romagnese
             Romanese  ->  Romanese
                romeo  ->  romeos
                 roof  ->  roofs
              rostrum  ->  rostrums|rostra
               ruckus  ->  ruckuses
               salmon  ->  salmon
            Sangirese  ->  Sangirese
                 sank  ->  sank
           Sarawakese  ->  Sarawakese
              sarcoma  ->  sarcomas|sarcomata
            sassafras  ->  sassafrases
                  saw  ->  saws                         # NOUN FORM
                  saw  ->  saw                          # VERB FORM (1st/2nd pers.)
                 saws  ->  saw                          # VERB FORM (3rd pers.)
                scarf  ->  scarves
               schema  ->  schemas|schemata
             scissors  ->  scissors
             Scotsman  ->  Scotsmen
             sea-bass  ->  sea-bass
                 self  ->  selves
               Selman  ->  Selmans
           Senegalese  ->  Senegalese
               seraph  ->  seraphs|seraphim
               series  ->  series
            shall eat  ->  shall eat
              Shavese  ->  Shavese
            Shawanese  ->  Shawanese
                  she  ->  they
                sheaf  ->  sheaves
               shears  ->  shears
                sheep  ->  sheep
                shelf  ->  shelves
          should have  ->  should have
              Siamese  ->  Siamese
              Sienese  ->  Sienese
            Sikkimese  ->  Sikkimese
                silex  ->  silices
              simplex  ->  simplexes|simplices
           Singhalese  ->  Singhalese
            Sinhalese  ->  Sinhalese
                sinus  ->  sinuses|sinus
                 size  ->  sizes
                sizes  ->  size                         #VERB FORM
             smallpox  ->  smallpox
                Smith  ->  Smiths
           Sogdianese  ->  Sogdianese
            soliloquy  ->  soliloquies
                 solo  ->  solos|soli
                 soma  ->  somas|somata
       son of a bitch  ->  sons of bitches
              Sonaman  ->  Sonamans
              soprano  ->  sopranos|soprani
               sought  ->  sought
              species  ->  species
             spectrum  ->  spectrums|spectra
             speculum  ->  speculums|specula
                spent  ->  spent
         spermatozoon  ->  spermatozoa
               sphinx  ->  sphinxes|sphinges
              stadium  ->  stadiums|stadia
               stamen  ->  stamens|stamina
               status  ->  statuses|status
               stereo  ->  stereos
               stigma  ->  stigmas|stigmata
             stimulus  ->  stimuli
                stoma  ->  stomas|stomata
               storey  ->  storeys
                story  ->  stories
              stratum  ->  strata
               strife  ->  strifes
                stylo  ->  stylos
               stylus  ->  styluses|styli
             succubus  ->  succubuses|succubi
             Sudanese  ->  Sudanese
               suffix  ->  suffixes
            Sundanese  ->  Sundanese
             superior  ->  superiors
      Surgeon-General  ->  Surgeons-General
              surplus  ->  surpluses
            Swahilese  ->  Swahilese
                swine  ->  swines|swine
              syringe  ->  syringes
               syrinx  ->  syrinxes|syringes
              tableau  ->  tableaus|tableaux
              Tacoman  ->  Tacomans
               tattoo  ->  tattoos
                tempo  ->  tempos|tempi
           Tenggerese  ->  Tenggerese
            testatrix  ->  testatrixes|testatrices
               testes  ->  testes
               testis  ->  testes
                 that  ->  those
                their  ->  their                        # POSSESSIVE FORM (GENDER-INCLUSIVE)
             themself  ->  themselves                   # ugly but gaining currency
                 they  ->  they                         # for indeterminate gender
                 this  ->  these
              thought  ->  thought                      # VERB FORM
              thought  ->  thoughts                     # NOUN FORM
                Times  ->  Timeses
             Timorese  ->  Timorese
             Tirolese  ->  Tirolese
               to her  ->  to them
           to herself  ->  to themselves
               to him  ->  to them
           to himself  ->  to themselves
                to it  ->  to them
                to it  ->  to them                      # ACCUSATIVE
            to itself  ->  to themselves
                to me  ->  to us
            to myself  ->  to ourselves
              to them  ->  to them                      # for indeterminate gender
          to themself  ->  to themselves                # ugly but gaining currency
               to you  ->  to you
          to yourself  ->  to yourselves
            Tocharese  ->  Tocharese
               tomato  ->  tomatoes
            Tonkinese  ->  Tonkinese
          tonsillitis  ->  tonsillitises
                tooth  ->  teeth
             Torinese  ->  Torinese
                torus  ->  toruses|tori
            trapezium  ->  trapeziums|trapezia
               trauma  ->  traumas|traumata
              travois  ->  travois
              trellis  ->  trellises
                tries  ->  try
               trilby  ->  trilbys
             trousers  ->  trousers
            trousseau  ->  trousseaus|trousseaux
                trout  ->  trout
                  try  ->  tries
                 tuna  ->  tuna
                 turf  ->  turfs|turves
             Tyrolese  ->  Tyrolese
            ultimatum  ->  ultimatums|ultimata
            umbilicus  ->  umbilicuses|umbilici
                umbra  ->  umbras|umbrae
               uterus  ->  uteruses|uteri
               vacuum  ->  vacuums|vacua
               vellum  ->  vellums
                velum  ->  velums|vela
           Vermontese  ->  Vermontese
             Veronese  ->  Veronese
             vertebra  ->  vertebrae
               vertex  ->  vertexes|vertices
             Viennese  ->  Viennese
           Vietnamese  ->  Vietnamese
                virus  ->  viruses
                vixen  ->  vixens
               vortex  ->  vortexes|vortices
               walrus  ->  walruses
                  was  ->  were
       was faced with  ->  were faced with
           was hoping  ->  were hoping
           Wenchowese  ->  Wenchowese
                 were  ->  were
           were found  ->  were found
                wharf  ->  wharves
              whiting  ->  whiting
           Whitmanese  ->  Whitmanese
                 wife  ->  wives
           wildebeest  ->  wildebeests|wildebeest
                 will  ->  will                         # VERB FORM
                 will  ->  wills                        # NOUN FORM
             will eat  ->  will eat                     # VERB FORM
                wills  ->  will                         # VERB FORM
                 wish  ->  wishes
             with him  ->  with them
              with it  ->  with them                    # ACCUSATIVE
                 wolf  ->  wolves
                woman  ->  women
   woman of substance  ->  women of substance
              woman's  ->  women's                      # POSSESSIVE FORM
            woodlouse  ->  woodlice
              Yakiman  ->  Yakimans
             Yengeese  ->  Yengeese
            Yokohaman  ->  Yokohamans
                  you  ->  you
                 your  ->  your                         # POSSESSIVE FORM
             yourself  ->  yourselves
                Yuman  ->  Yumans
            Yunnanese  ->  Yunnanese
                 zoon  ->  zoa

	an A.B.C
	an AI
	an AGE
	an agendum
	an aide-de-camp
	an albino
	 a B.L.T. sandwich
	 a BMW
	 a BLANK
	 a bacterium
	 a Burmese restaurant
	 a C.O.
	 a CCD
	 a COLON
	 a cameo
	 a CAPITAL
	 a D.S.M.
	 a DNR
	 a DINNER
	 a dynamo
	an E.K.G.
	an ECG
	an EGG
	an embryo
	an erratum
	 a eucalyptus
	an Euler number
	 a eulogy
	 a euphemism
	 a euphoria
	 a ewe
	 a ewer
	an extremum
	an eye
	an F.B.I. agent
	an FSM
	 a FACT
	 a FAQ
	an F.A.Q.
	 a fish
	 a G-string
	 a GSM phone
	 a GOD
	 a genus
	 a Governor General
	an H-Bomb
	an H.M.S Ark Royal
	an HSL colour space
	 a HAL 9000
	an H.A.L. 9000
	 a has-been
	 a height
	an heir
	 a honed blade
	an honest man
	 a honeymoon
	an honorarium
	an honorary degree
	an honoree
	an honorific
	 a Hough transform
	 a hound
	an hour
	an hourglass
	 a houri
	 a house
	an I.O.U.
	an IQ
	an IDEA
	an inferno
	an Inspector General
	 a jumbo
	 a knife
	an L.E.D.
	 a LED
	an LCD
	 a lady in waiting
	 a leaf
	an M.I.A.
	 a MIASMA
	an MTV channel
	 a Major General
	an N.C.O.
	an NCO
	 a NATO country
	 a note
	an O.K.
	an OK
	an OLE
	an octavo
	an octopus
	an okay
	 a once-and-future-king
	an oncologist
	 a one night stand
	an onerous task
	an opera
	an optimum
	an opus
	an ox
	 a Ph.D.
	 a PET
	 a P.E.T. scan
	 a plateau
	 a quantum
	an R.S.V.P.
	an RSVP
	 a REST
	 a reindeer
	an S.O.S.
	 a SUM
	an SST
	 a salmon
	 a T.N.T. bomb
	 a TNT bomb
	 a TENT
	 a thought
	 a tomato
	 a U-boat
	 a U.F.O.
	 a UFO
	 a ubiquity
	 a unicorn
	an unidentified flying object
	 a uniform
	 a unimodal system
	an unimpressive record
	an uninformed opinion
	an uninvited guest
	 a union
	 a uniplex
	 a uniprocessor
	 a unique opportunity
	 a unisex hairdresser
	 a unison
	 a unit
	 a unitarian
	 a united front
	 a unity
	 a univalent bond
	 a univariate statistic
	 a universe
	an unordered meal
	 a uranium atom
	an urban myth
	an urbane miss
	an urchin
	 a urea detector
	 a urethane monomer
	an urge
	an urgency
	 a urinal
	an urn
	 a usage
	 a use
	an usher
	 a usual suspect
	 a usurer
	 a usurper
	 a utensil
	 a utility
	an utmost urgency
	 a utopia
	an utterance
	 a V.I.P.
	 a VIPER
	 a viper
	an X-ray
	an X.O.
	 a XYLAPHONE
	an XY chromosome
	 a xenophobe
	 a Y-shaped pipe
	 a Y.Z. plane
	 a YMCA
	an YBLENT eye
	an yblent eye
	an yclad body
	 a yellowing
	 a yield
	 a youth
	 a youth
	an ypsiliform junction
	an yttrium atom
	 a zoo

