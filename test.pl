# Before `make install' is performed this script should be runnable with
# `make test'. After `make install' it should work as `perl test.pl'

######################### We start with some black magic to print on failure.

# Change 1..1 below to 1..last_test_to_print .
# (It may become useful if the test is moved to ./t subdirectory.)

BEGIN { $| = 1; print "1..776\n"; }
END {print "not ok 1\n" unless $loaded;}
use Lingua::EN::Inflect qw( :ALL );
$loaded = 1;
print "ok 1\n";

sub debug { $D || 0 }

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


	elsif (/^\s*(an?)\s+(.*?)\s*$/)
	{
		$article = $1;
		$word    = $2;
		$Aword   = A($word);

		ok ("$article $word" eq $Aword);
	}
}

__DATA__
                    a  ->  as                           # NOUN FORM
                    a  ->  some                         # INDEFINITE ARTICLE
       A.C.R.O.N.Y.M.  ->  A.C.R.O.N.Y.M.s
             abscissa  ->  abscissas|abscissae
            acropolis  ->  acropolises
                adieu  ->  adieus|adieux
     adjutant general  ->  adjutant generals
                aegis  ->  aegises
             afflatus  ->  afflatuses
               afreet  ->  afreets|afreeti
                afrit  ->  afrits|afriti
              agendum  ->  agenda
         aide-de-camp  ->  aides-de-camp
               albino  ->  albinos
                album  ->  albums
                 alga  ->  algae
                alias  ->  aliases
                 alto  ->  altos|alti
               alumna  ->  alumnae
              alumnus  ->  alumni
             alveolus  ->  alveoli
                   am  ->  are
             am going  ->  are going
  ambassador-at-large  ->  ambassadors-at-large
               amoeba  ->  amoebas|amoebae
                   an  ->  some                         # INDEFINITE ARTICLE
             analysis  ->  analyses
             anathema  ->  anathemas|anathemata
              antenna  ->  antennas|antennae
                 anus  ->  anuses
                 apex  ->  apexes|apices
               apex's  ->  apexes'|apices'              # POSSESSIVE FORM
             aphelion  ->  aphelia
            apparatus  ->  apparatuses|apparatus
                apple  ->  apples
             aquarium  ->  aquariums|aquaria
          archipelago  ->  archipelagos
                  are  ->  are
             are made  ->  are made
            armadillo  ->  armadillos
             arpeggio  ->  arpeggios
            arthritis  ->  arthritises
             asbestos  ->  asbestoses
            asparagus  ->  asparaguses
                  ass  ->  asses
               asylum  ->  asylums
            asyndeton  ->  asyndeta
                at it  ->  at them                      # ACCUSATIVE
                  ate  ->  ate
                atlas  ->  atlases
     attorney general  ->  attorneys general
               aurora  ->  auroras|aurorae
             aviatrix  ->  aviatrixes|aviatrices
           aviatrix's  ->  aviatrixes'|aviatrices'
                  axe  ->  axes
                 axis  ->  axes
             bacillus  ->  bacilli
            bacterium  ->  bacteria
               bamboo  ->  bamboos
                banjo  ->  banjoes
                 bass  ->  basses                       # INSTRUMENT, NOT FISH
                basso  ->  bassos|bassi
               bathos  ->  bathoses
                 beau  ->  beaus|beaux
                 beef  ->  beefs|beeves
           beneath it  ->  beneath them                 # ACCUSATIVE
                 bent  ->  bent                         # VERB FORM
                 bent  ->  bents                        # NOUN FORM
            Bhutanese  ->  Bhutanese
                 bias  ->  biases
               biceps  ->  biceps
                bison  ->  bison
                bonus  ->  bonuses
                 boss  ->  bosses
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
                 buoy  ->  buoys
               bureau  ->  bureaus|bureaux
              Burmese  ->  Burmese
             bursitis  ->  bursitises
                  bus  ->  buses
                 buzz  ->  buzzes
               buzzes  ->  buzz                         # VERB FORM
                by it  ->  by them                      # ACCUSATIVE
               caddis  ->  caddises
                 cake  ->  cakes
                 calf  ->  calves
               callus  ->  calluses
                cameo  ->  cameos
               campus  ->  campuses
                  can  ->  cans                         # NOUN FORM
                  can  ->  can                          # VERB FORM (all pers.)
          candelabrum  ->  candelabra
             cannabis  ->  cannabises
                canto  ->  cantos
               cantus  ->  cantus
               canvas  ->  canvases
              CAPITAL  ->  CAPITALS
            carcinoma  ->  carcinomas|carcinomata
                 care  ->  cares
                cargo  ->  cargoes
                 carp  ->  carp
                  cat  ->  cats
              catfish  ->  catfish
             chairman  ->  chairmen
              chamois  ->  chamois
                chaos  ->  chaoses
              chapeau  ->  chapeaus|chapeaux
             charisma  ->  charismas|charismata
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
           conspectus  ->  conspectuses
            contralto  ->  contraltos|contralti
          contretemps  ->  contretemps
            conundrum  ->  conundrums
                corps  ->  corps
               corpus  ->  corpuses|corpora
               cortex  ->  cortexes|cortices
               cosmos  ->  cosmoses
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
               domino  ->  dominoes
                drama  ->  dramas|dramata
                 drum  ->  drums
                dwarf  ->  dwarves
               dynamo  ->  dynamos
                edema  ->  edemas|edemata
                eland  ->  eland
                  elf  ->  elves
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
             factotum  ->  factotums
                  fax  ->  faxes
                ferry  ->  ferries
                fetus  ->  fetuses
               fiance  ->  fiances
              fiancee  ->  fiancees
               fiasco  ->  fiascos
                 fish  ->  fish
                 fizz  ->  fizzes
             flamingo  ->  flamingoes
             flounder  ->  flounder
                focus  ->  focuses|foci
               foetus  ->  foetuses
                folio  ->  folios
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
              gallows  ->  gallows
             ganglion  ->  ganglions|ganglia
                  gas  ->  gases
               gateau  ->  gateaus|gateaux
                 gave  ->  gave
        generalissimo  ->  generalissimos
                genie  ->  genies|genii
               genius  ->  geniuses|genii
                genus  ->  genera
	       German  ->  Germans
               ghetto  ->  ghettos
              glottis  ->  glottises
                goose  ->  geese
     Governor General  ->  Governors General
                  goy  ->  goys|goyim
             graffiti  ->  graffiti
             graffito  ->  graffiti
                guano  ->  guanos
                gumma  ->  gummas|gummata
            gymnasium  ->  gymnasiums|gymnasia
                  had  ->  had
          had thought  ->  had thought
         handkerchief  ->  handkerchiefs
            harmonium  ->  harmoniums
                  has  ->  have
           has become  ->  have become
             has been  ->  have been
             has-been  ->  has-beens
                 have  ->  have
        have conceded  ->  have conceded
                   he  ->  they
         headquarters  ->  headquarters
            hepatitis  ->  hepatitises
                  her  ->  their                        # POSSESSIVE ADJ
                  her  ->  them                         # PRONOUN
                 hero  ->  heroes
               herpes  ->  herpes
                 hers  ->  theirs                       # POSSESSIVE NOUN
              herself  ->  themselves
               hiatus  ->  hiatuses|hiatus
            highlight  ->  highlights
              hijinks  ->  hijinks
                  him  ->  them
              himself  ->  themselves
         hippopotamus  ->  hippopotamuses
                  his  ->  their                        # POSSESSIVE ADJ
                  his  ->  theirs                       # POSSESSIVE NOUN
           honorarium  ->  honorariums|honoraria
                 hoof  ->  hoofs|hooves
                house  ->  houses
            housewife  ->  housewives
               hubris  ->  hubrises
	        human  ->  humans
                hydra  ->  hydras|hydrae
           hyperbaton  ->  hyperbata
            hyperbola  ->  hyperbolas|hyperbolae
                    I  ->  we
                 ibis  ->  ibises
            ignoramus  ->  ignoramuses
              impetus  ->  impetuses|impetus
              incubus  ->  incubuses|incubi
                index  ->  indexes|indices
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
                jerry  ->  jerries
                Jerry  ->  Jerrys
                 jinx  ->  jinxes
               jinxes  ->  jinx                         # VERB FORM
                Jones  ->  Joneses
                jumbo  ->  jumbos
                knife  ->  knife                        # VERB FORM (1st/2nd pers.)
                knife  ->  knives                       # NOUN FORM
               knifes  ->  knife                        # VERB FORM (3rd pers.)
               lacuna  ->  lacunas|lacunae
      lady in waiting  ->  ladies in waiting
               larynx  ->  larynxes|larynges
                latex  ->  latexes|latices
                 leaf  ->  leaf                         # VERB FORM (1st/2nd pers.)
                 leaf  ->  leaves                       # NOUN FORM
                leafs  ->  leaf                         # VERB FORM (3rd pers.)
             Lebanese  ->  Lebanese
                lemma  ->  lemmas|lemmata
                 lens  ->  lenses
   Lieutenant General  ->  Lieutenant Generals
                 life  ->  lives
                lingo  ->  lingos
                 loaf  ->  loaves
                locus  ->  loci
             lothario  ->  lotharios
                louse  ->  lice
              lumbago  ->  lumbagos
                lumen  ->  lumens|lumina
              lustrum  ->  lustrums|lustra
               lyceum  ->  lyceums
             lymphoma  ->  lymphomas|lymphomata
                 lynx  ->  lynxes
               M.I.A.  ->  M.I.A.s
             mackerel  ->  mackerel
                 made  ->  made
                magma  ->  magmas|magmata
              magneto  ->  magnetos
        Major General  ->  Major Generals
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
        metamorphosis  ->  metamorphoses
           metropolis  ->  metropolises
                 mews  ->  mews
               miasma  ->  miasmas|miasmata
               milieu  ->  milieus|milieux
            millenium  ->  milleniums|millenia
              minimum  ->  minimums|minima
                 minx  ->  minxes
                 miss  ->  misses                       # NOUN FORM
                 miss  ->  miss                         # VERB FORM (1st/2nd pers.)
               misses  ->  miss                         # VERB FORM (3rd pers.)
             mittamus  ->  mittamuses
             momentum  ->  momentums|momenta
                money  ->  monies
             mongoose  ->  mongooses
        mother-in-law  ->  mothers-in-law
                mouse  ->  mice
                mumps  ->  mumps
                murex  ->  murices
               museum  ->  museums
            mustachio  ->  mustachios
                   my  ->  our                          # POSSESSIVE FORM
               myself  ->  ourselves
               mythos  ->  mythoi
           nasturtium  ->  nasturtiums
               nebula  ->  nebulas|nebulae
             neuritis  ->  neuritises
             neurosis  ->  neuroses
                 news  ->  news
                nexus  ->  nexus
               nimbus  ->  nimbuses|nimbi
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
             parabola  ->  parabolas|parabolae
               pathos  ->  pathoses
              pegasus  ->  pegasuses
               pelvis  ->  pelvises
             pendulum  ->  pendulums
                penis  ->  penises|penes
           perihelion  ->  perihelia
            petroleum  ->  petroleums
              phalanx  ->  phalanxes|phalanges
                  PhD  ->  PhDs
           phenomenon  ->  phenomena
             philtrum  ->  philtrums
                photo  ->  photos
               phylum  ->  phylums|phyla
               pincer  ->  pincers
              pincers  ->  pincers
              plateau  ->  plateaus|plateaux
                 play  ->  plays
               plexus  ->  plexuses|plexus
               pliers  ->  pliers
                plies  ->  ply                          # VERB FORM
                polis  ->  polises
             pontifex  ->  pontifexes|pontifices
          portmanteau  ->  portmanteaus|portmanteaux
           Portuguese  ->  Portuguese
               potato  ->  potatoes
                  pox  ->  pox
              premium  ->  premiums
          prima donna  ->  prima donnas|prime donne
                  pro  ->  pros
          proceedings  ->  proceedings
         prolegomenon  ->  prolegomena
                proof  ->  proofs
          prosecutrix  ->  prosecutrixes|prosecutrices
           prospectus  ->  prospectuses|prospectus
            protozoan  ->  protozoans
            protozoon  ->  protozoa
                  put  ->  put
              quantum  ->  quantums|quanta
quartermaster general  ->  quartermaster generals
               quarto  ->  quartos
               quorum  ->  quorums
               rabies  ->  rabies
               radius  ->  radiuses|radii
                rebus  ->  rebuses
             reindeer  ->  reindeer
                rhino  ->  rhinos
           rhinoceros  ->  rhinoceroses
                romeo  ->  romeos
                 roof  ->  roofs
              rostrum  ->  rostrums|rostra
               ruckus  ->  ruckuses
               salmon  ->  salmon
                 sank  ->  sank
              sarcoma  ->  sarcomas|sarcomata
            sassafras  ->  sassafrases
                  saw  ->  saws                         # NOUN FORM
                  saw  ->  saw                          # VERB FORM (1st/2nd pers.)
                 saws  ->  saw                          # VERB FORM (3rd pers.)
                scarf  ->  scarves
               schema  ->  schemas|schemata
             scissors  ->  scissors
             sea-bass  ->  sea-bass
                 self  ->  selves
           Senegalese  ->  Senegalese
               seraph  ->  seraphs|seraphim
               series  ->  series
            shall eat  ->  shall eat
                  she  ->  they
                sheaf  ->  sheaves
               shears  ->  shears
                sheep  ->  sheep
                shelf  ->  shelves
          should have  ->  should have
              Siamese  ->  Siamese
                silex  ->  silices
              simplex  ->  simplexes|simplices
            Sinhalese  ->  Sinhalese
                sinus  ->  sinuses|sinus
                 size  ->  sizes
                sizes  ->  size                         #VERB FORM
             smallpox  ->  smallpox
                Smith  ->  Smiths
            soliloquy  ->  soliloquies
                 solo  ->  solos|soli
                 soma  ->  somas|somata
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
             superior  ->  superiors
      Surgeon-General  ->  Surgeons-General
              surplus  ->  surpluses
                swine  ->  swine
              syringe  ->  syringes
               syrinx  ->  syrinxes|syringes
              tableau  ->  tableaus|tableaux
               tattoo  ->  tattoos
                tempo  ->  tempos|tempi
            testatrix  ->  testatrixes|testatrices
               testes  ->  testes
               testis  ->  testes
                 that  ->  those
                their  ->  their                        # POSSESSIVE FORM (GENDER-INCLUSIVE)
             themself  ->  themselves                   # ugly but gaining currency
                 they  ->  they                         # for indeterminate gender
                 this  ->  these
              thought  ->  thoughts                     # NOUN FORM
              thought  ->  thought                      # VERB FORM
                Times  ->  Timeses
               to her  ->  to them
           to herself  ->  to themselves
               to him  ->  to them
           to himself  ->  to themselves
                to it  ->  to them                      # ACCUSATIVE
                to it  ->  to them
            to itself  ->  to themselves
                to me  ->  to us
            to myself  ->  to ourselves
              to them  ->  to them                      # for indeterminate gender
          to themself  ->  to themselves                # ugly but gaining currency
               to you  ->  to you
          to yourself  ->  to yourselves
               tomato  ->  tomatoes
          tonsillitis  ->  tonsillitises
                tooth  ->  teeth
                torus  ->  toruses|tori
            trapezium  ->  trapeziums|trapezia
               trauma  ->  traumas|traumata
              travois  ->  travois
              trellis  ->  trellises
                tries  ->  try
               trilby  ->  trilbys
            trousseau  ->  trousseaus|trousseaux
                trout  ->  trout
                  try  ->  tries
                 tuna  ->  tuna
                 turf  ->  turfs|turves
            ultimatum  ->  ultimatums|ultimata
            umbilicus  ->  umbilicuses|umbilici
               uterus  ->  uteruses|uteri
               vacuum  ->  vacuums|vacua
               vellum  ->  vellums
                velum  ->  velums|vela
             vertebra  ->  vertebrae
               vertex  ->  vertexes|vertices
           Vietnamese  ->  Vietnamese
                virus  ->  viruses
                vixen  ->  vixens
               vortex  ->  vortexes|vortices
               walrus  ->  walruses
                  was  ->  were
       was faced with  ->  were faced with
           was hoping  ->  were hoping
                 were  ->  were
           were found  ->  were found
                wharf  ->  wharves
              whiting  ->  whiting
                 wife  ->  wives
           wildebeest  ->  wildebeest
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
                  you  ->  you
                 your  ->  your                         # POSSESSIVE FORM
             yourself  ->  yourselves
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
