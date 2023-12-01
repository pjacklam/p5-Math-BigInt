# -*- mode: perl; -*-

use strict;
use warnings;

use Test::More;

use Math::BigFloat;

my $class = 'Math::BigFloat';

my ($x, $test);

################################################

my $entries =
  [

   [
    '$x = Math::BigFloat -> bone(); $x -> bexp();',
    '2.718281828459045235360287471352662497757',
   ],

   # Test accuracy.

   [
    '$x = Math::BigFloat -> bone(); $x -> bexp(8);',
    '2.7182818',
   ],

   [
    '$x = Math::BigFloat -> bone(); $x -> accuracy(8); $x -> bexp();',
    '2.7182818',
   ],

   [
    '$x = Math::BigFloat -> bone(); $x -> accuracy(8); $x -> bexp(4);',
    '2.718',
   ],

   [
    '$x = Math::BigFloat -> bone(); $x -> accuracy(4); $x -> bexp(8);',
    '2.7182818',
   ],

   # Test precision.

   [
    '$x = Math::BigFloat -> bone(); $x -> bexp(undef, -7);',
    '2.7182818',
   ],

   [
    '$x = Math::BigFloat -> bone(); $x -> precision(-7); $x -> bexp();',
    '2.7182818',
   ],

   [
    '$x = Math::BigFloat -> bone(); $x -> precision(-7); $x -> bexp(undef, -3);',
    '2.718',
   ],

   [
    '$x = Math::BigFloat -> bone(); $x -> precision(-3); $x -> bexp(undef, -7);',
    '2.7182818',
   ],

   # Test accuracy for large positive argument.

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> bexp();',
    '413616930694187906441132159048439731462700000000000000',
   ],

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> bexp(60);',
    '413616930694187906441132159048439731462689955622231644.661237',
   ],

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> accuracy(60); $x -> bexp();',
    '413616930694187906441132159048439731462689955622231644.661237',
   ],

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> accuracy(40); $x -> bexp(60);',
    '413616930694187906441132159048439731462689955622231644.661237',
   ],

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> accuracy(80); $x -> bexp(60);',
    '413616930694187906441132159048439731462689955622231644.661237',
   ],

   # Test precision for large positive argument.

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> bexp();',
    '413616930694187906441132159048439731462700000000000000',
   ],

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> bexp(undef, -6);',
    '413616930694187906441132159048439731462689955622231644.661237',
   ],

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> precision(-6); $x -> bexp();',
    '413616930694187906441132159048439731462689955622231644.661237',
   ],

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> precision(-5); $x -> bexp(undef, -6);',
    '413616930694187906441132159048439731462689955622231644.661237',
   ],

   [
    '$x = Math::BigFloat -> new("123.45678"); $x -> precision(-9); $x -> bexp(undef, -6);',
    '413616930694187906441132159048439731462689955622231644.661237',
   ],

   # Test accuracy for large negative argument.

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> bexp();',
    '0.000000000000000000000000000000000000000000000000000002417696002728091119130671660435314456803',
   ],

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> bexp(60);',
    '0.00000000000000000000000000000000000000000000000000000241769600272809111913067166043531445680296248978007951017367',
   ],

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> accuracy(60); $x -> bexp();',
    '0.00000000000000000000000000000000000000000000000000000241769600272809111913067166043531445680296248978007951017367',
   ],

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> accuracy(40); $x -> bexp(60);',
    '0.00000000000000000000000000000000000000000000000000000241769600272809111913067166043531445680296248978007951017367',
   ],

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> accuracy(80); $x -> bexp(60);',
    '0.00000000000000000000000000000000000000000000000000000241769600272809111913067166043531445680296248978007951017367',
   ],

   # Test precision for large negative argument.

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> bexp(undef, -113);',
    '0.00000000000000000000000000000000000000000000000000000241769600272809111913067166043531445680296248978007951017367',
   ],

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> precision(-113); $x -> bexp();',
    '0.00000000000000000000000000000000000000000000000000000241769600272809111913067166043531445680296248978007951017367',
   ],

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> precision(-93); $x -> bexp(undef, -113);',
    '0.00000000000000000000000000000000000000000000000000000241769600272809111913067166043531445680296248978007951017367',
   ],

   [
    '$x = Math::BigFloat -> new("-123.45678"); $x -> precision(-133); $x -> bexp(undef, -113);',
    '0.00000000000000000000000000000000000000000000000000000241769600272809111913067166043531445680296248978007951017367',
   ],

  ];

for my $entry (@$entries) {
    my ($test, $want) = @$entry;

    note "\n", $test, "\n\n";
    eval $test;
    die $@ if $@;

    is($x, $want, 'value of $x');
}

my $accu       = 16;
my $tol        = 1e-14;
my $max_relerr = 0;

while (<DATA>) {
    s/#.*$//;                   # remove comments
    s/\s+$//;                   # remove trailing whitespace
    next unless length;         # skip empty lines

    my ($in, $wanted) = split /:/;
    my $test = qq|\$x = $class -> new("$in", 20) -> bexp() -> bsstr();|;
    note "\n", $test, "\n\n";
    eval $test;
    die $@ if $@;

    is($x, $wanted, $test);
}

done_testing();

__DATA__
-120.00:76676480737219996324e-72
-119.75:9845455012875848912e-71
-119.50:12641814475388325097e-71
-119.25:16232411099449616144e-71
-119.00:20842828425817513239e-71
-118.75:26762721454411435331e-71
-118.50:34364014567198602057e-71
-118.25:44124268123710803338e-71
-118.00:56656681763589393363e-71
-117.75:72748619409637634865e-71
-117.50:93411076350917881994e-71
-117.25:11994219623473776769e-70
-117.00:15400882849875201985e-70
-116.75:19775125018670097804e-70
-116.50:25391763142150053927e-70
-116.25:32603669249035658995e-70
-116.00:41863939993042315515e-70
-115.75:53754362993756764505e-70
-115.50:69021968341842640327e-70
-115.25:88625961660742597394e-70
-115.00:11379798735078681489e-69
-114.75:14611950812632037462e-69
-114.50:18762116230810634694e-69
-114.25:24091034111210462891e-69
-114.00:3093350011308560843e-68
-113.75:39719400372315046683e-69
-113.50:51000719613649062059e-69
-113.25:65486220253290406784e-69
-113.00:84085971248036430243e-69
-112.75:10796852426935342718e-68
-112.50:1386343293641170635e-67
-112.25:17801000252898600736e-68
-112.00:22856936767186717347e-68
-111.75:29348887756692283505e-68
-111.50:37684717831108563734e-68
-111.25:48388135515849135057e-68
-111.00:62131595868481088366e-68
-110.75:79778548274500786728e-68
-110.50:10243768369090897206e-67
-110.25:13153258948574645287e-67
-110.00:16889118802245323352e-67
-109.75:2168605780754182068e-66
-109.50:27845449412643335817e-67
-109.25:35754264784926785636e-67
-109.00:4590938473882945758e-66
-108.75:58948816869153334016e-67
-108.50:75691779143663974075e-67
-108.25:97190168254779634597e-67
-108.00:12479464629129512485e-66
-107.75:16023949770457953511e-66
-107.50:20575158780995713264e-66
-107.25:26419026827184463853e-66
-107.00:33922701930260152026e-66
-106.75:43557611481176344062e-66
-106.50:55929080232040207716e-66
-106.25:71814360549907552883e-66
-106.00:92211464229258749255e-66
-105.75:11840186378036073414e-65
-105.50:1520310024771828978e-64
-105.25:19521167130521982812e-65
-105.00:25065674758999531731e-65
-104.75:32184963476983777833e-65
-104.50:41326311139613836304e-65
-104.25:53064033881209908793e-65
-104.00:68135568215452985134e-65
-103.75:87487801369103053668e-65
-103.50:11233656060805690838e-64
-103.25:14424299904402799765e-64
-103.00:18521167695179754623e-64
-102.75:23781650067346720982e-64
-102.50:30536243137246928736e-64
-102.25:39209312318381673938e-64
-102.00:50345753587649823968e-64
-101.75:64645227228840420972e-64
-101.50:83006114829385553249e-64
-101.25:10658196118143230263e-63
-101.00:13685394711738530002e-63
-100.75:17572394647276279739e-63
-100.50:22563401359170363132e-63
-100.25:28971980832101478072e-63
-100.00:3720075976020835963e-62
-99.75:47766721052202104946e-63
-99.50:61333683902860921145e-63
-99.25:78754009030365216655e-63
-99.00:10112214926104485299e-62
-98.75:12984340984127310697e-62
-98.50:16672223842559789452e-62
-98.25:21407559166554132125e-62
-98.00:274878500791021493e-60
-97.75:35295098151669304968e-62
-97.50:4531980311123191359e-61
-97.25:58191779074105960769e-62
-97.00:74719723373429901606e-62
-96.75:95942023939361106199e-62
-96.50:12319199726660341309e-61
-96.25:15818165562284555743e-61
-96.00:20310926627348109257e-61
-95.75:26079746025994799787e-61
-95.50:33487056758138442847e-61
-95.25:42998232007514763574e-61
-95.00:55210822770285327317e-61
-94.75:70892099713288663036e-61
-94.50:91027257874224394149e-61
-94.25:1168813127218934748e-59
-94.00:15007857627073948875e-60
-93.75:19270470643193926345e-60
-93.50:24743774097395970922e-60
-93.25:31771634845836206e-57
-93.00:40795586671775601577e-60
-92.75:52382570175247539595e-60
-92.50:67260551496447081483e-60
-92.25:86364257661872757829e-60
-92.00:11089390193121363795e-59
-91.75:14239058863535613154e-59
-91.50:18283313490492594385e-59
-91.25:23476239223062358704e-59
-91.00:30144087850653745533e-59
-90.75:38705774963107561194e-59
-90.50:4969919882522613803e-58
-90.25:63815034480607903936e-59
-90.00:81940126239905154304e-59
-89.75:10521320473864035558e-58
-89.50:13509642905558533942e-58
-89.25:17346724861112387118e-58
-89.00:22273635617957437392e-58
-88.75:28599914255498718833e-58
-88.50:36723016819150420212e-58
-88.25:47153286973240557166e-58
-88.00:60546018954011858845e-58
-87.75:77742627216208970853e-58
-87.50:9982350930569247556e-57
-87.25:12817592313147442053e-57
-87.00:16458114310822736512e-57
-86.75:21132637085848645513e-57
-86.50:27134843139867625959e-57
-86.25:34841828269425031899e-57
-86.00:44737793061811207346e-57
-85.75:57444463377882085421e-57
-85.50:73760151025188750427e-57
-85.25:94709908655068727477e-57
-85.00:12160992992528255644e-56
-84.75:15615024094567797714e-56
-84.50:20050087819616539644e-56
-84.25:2574482236720893743e-55
-84.00:33057006267607342985e-56
-83.75:4244603624721380045e-55
-83.50:54501789379071678925e-56
-83.25:69981682817690035738e-56
-83.00:89858259440493806697e-56
-82.75:11538028902091524009e-55
-82.50:14815122368763273507e-55
-82.25:19022993672831141914e-55
-82.00:24426007377405276794e-55
-81.75:313636143007906581e-53
-81.50:40271677921406332541e-55
-81.25:51709858023748284986e-55
-81.00:66396771995807344007e-55
-80.75:85255142828637489655e-55
-80.50:10946977029531416782e-54
-80.25:14056196741815211947e-54
-80.00:18048513878454151723e-54
-79.75:23174750553376577323e-54
-79.50:29756968735933827241e-54
-79.25:38208704180521528511e-54
-79.00:49060947306492805661e-54
-78.75:62995503308314753001e-54
-78.50:80887827384912847916e-54
-78.25:10386202626287882612e-53
-78.00:13336148155022613415e-53
-77.75:17123953191762366003e-53
-77.50:21987591132394052761e-53
-77.25:28232625865731963229e-53
-77.00:36251409191435592242e-53
-76.75:46547730792550907923e-53
-76.50:59768469426773994703e-53
-76.25:76744233860502015951e-53
-76.00:98541546861112580289e-53
-75.75:12652985076939468481e-52
-75.50:16246754435760975758e-52
-75.25:20861245634201399181e-52
-75.00:26786369618080779443e-52
-74.75:34394379410408031541e-52
-74.50:44163257354165448968e-52
-74.25:56706744926470254098e-52
-74.00:72812901783216438343e-52
-73.75:93493616552438082193e-52
-73.50:1200481799513882329e-50
-73.25:15414491428468624897e-51
-73.00:19792598779469045537e-51
-72.75:25414199895141024714e-51
-72.50:32632478610144010189e-51
-72.25:41900931944943973771e-51
-72.00:53801861600211384138e-51
-71.75:69082957759787620349e-51
-71.50:88704273623532943184e-51
-71.25:11389854190144032355e-50
-71.00:14624862272512309468e-50
-70.75:18778694873463448014e-50
-70.50:24112321509750858678e-50
-70.25:3096083367386663815e-49
-70.00:39754497359086468078e-50
-69.75:51045785036712720566e-50
-69.50:65544085401917930254e-50
-69.25:84160271569614584159e-50
-69.00:10806392777072784945e-49
-68.75:13875682988472282558e-49
-68.50:17816729631100128627e-49
-68.25:22877133688586173275e-49
-68.00:29374821117108029466e-49
-67.75:37718016925022505322e-49
-67.50:48430892398787308141e-49
-67.25:62186496792912045045e-49
-67.00:79849042456869788084e-49
-66.75:10252820001279939066e-48
-66.50:13164881474367883936e-48
-66.25:1690404242076995063e-47
-66.00:2170522011303639412e-47
-65.75:27870054299940703276e-48
-65.50:35785858085591342804e-48
-65.25:45949951339879806699e-48
-65.00:59000905415970613914e-48
-64.75:75758662161695690827e-48
-64.50:97276047749877143426e-48
-64.25:12490491774577263648e-47
-64.00:1603810890548637853e-46
-63.75:20593339470250524694e-47
-63.50:26442371294280543444e-47
-63.25:33952676819350548893e-47
-63.00:43596100000630809736e-47
-62.75:55978500469270422248e-47
-62.50:71877817390609886133e-47
-62.25:92292944425583250339e-47
-62.00:11850648642339810063e-46
-61.75:15216534061000392514e-46
-61.50:19538416488219240091e-46
-61.25:25087823372704348409e-46
-61.00:3221340285992516089e-45
-60.75:41362828030145487931e-46
-60.50:53110922496790953416e-46
-60.25:68195774389612347283e-46
-60.00:87565107626965203385e-46
-59.75:11243582380802092517e-45
-59.50:1443704555157235529e-44
-59.25:18537533430097598089e-45
-59.00:23802664086944006059e-45
-58.75:30563225672516617073e-45
-58.50:39243958579474627186e-45
-58.25:50390240267486375548e-45
-58.00:64702349256454603262e-45
-57.75:83079460964694902339e-45
-57.50:10667613948338532507e-44
-57.25:13697487445079347359e-44
-57.00:1758792202424311649e-43
-56.75:2258333890585027333e-43
-56.50:28997581148784881061e-44
-56.25:37233631217505104293e-44
-56.00:47808928838854690813e-44
-55.75:61387879773704975006e-44
-55.50:78823597906008507933e-44
-55.25:1012115031460895615e-42
-55.00:12995814250075030736e-43
-54.75:16686955807649079951e-43
-54.50:21426475384166537618e-43
-54.25:27512138983304074257e-43
-54.00:35326285722008070297e-43
-53.75:45359848744231624988e-43
-53.50:58243198684704939568e-43
-53.25:74785747460355176767e-43
-53.00:96026800545086760302e-43
-52.75:12330085258309566847e-42
-52.50:15832142861596320162e-42
-52.25:20328873834921066848e-42
-52.00:26102790696677048047e-42
-51.75:33516646701013648448e-42
-51.50:43036226246244865914e-42
-51.25:55259608338502480501e-42
-51.00:7095474162284704139e-41
-50.75:91107691678247206731e-42
-50.50:11698459177061964686e-41
-50.25:15021118919431522539e-41
-50.00:1928749847963917783e-40
-49.75:24765638272182875672e-41
-49.50:31799709001977494982e-41
-49.25:40831634601813076879e-41
-49.00:52428856633634639372e-41
-48.75:67319984485464577081e-41
-48.50:86440571130360945577e-41
-48.25:11099189036438807005e-40
-48.00:14251640827409351063e-40
-47.75:18299469051898320814e-40
-47.50:23496983374528170976e-40
-47.25:30170723868383468759e-40
-47.00:38739976286871871129e-40
-46.75:4974311419422387845e-39
-46.50:63871422930584223502e-40
-46.25:82012530442882373902e-40
-46.00:10530617357553812379e-39
-45.75:13521580340512197092e-39
-45.50:17362052831002947254e-39
-45.25:22293317120883141026e-39
-45.00:28625185805493936445e-39
-44.75:36755466131663374597e-39
-44.50:47194952715261234164e-39
-44.25:60599518825771562567e-39
-44.00:77811322411337965157e-39
-43.75:9991171568224242818e-38
-43.50:12828918236087848928e-38
-43.25:16472657083745665699e-38
-43.00:21151310375910804866e-38
-42.75:27158820118920621185e-38
-42.50:34872615319944467343e-38
-42.25:4477732441718301199e-37
-42.00:57495222642935598067e-38
-41.75:73825327211649850516e-38
-41.50:94793596535047559454e-38
-41.25:12171738729024408987e-37
-41.00:15628821893349887681e-37
-40.75:20067804543947086326e-37
-40.50:25767571091549809481e-37
-40.25:33086216207858245787e-37
-40.00:42483542552915889953e-37
-39.75:54549948428879222214e-37
-39.50:70043520261686452206e-37
-39.25:8993766029028821023e-36
-39.00:11548224173015785986e-36
-38.75:14828213355760043407e-36
-38.50:19039802832864523191e-36
-38.25:24447590766121310119e-36
-38.00:31391327920480296287e-36
-37.75:40307262913476245811e-36
-37.50:51755550058018685349e-36
-37.25:66455441729150705396e-36
-37.00:85330476257440657943e-36
-36.75:10956650033262367229e-35
-36.50:14068617124461467672e-35
-36.25:18064461965456931424e-35
-36.00:23195228302435693883e-35
-35.75:29783262686202286388e-35
-35.50:38242466280971353519e-35
-35.25:49104298701591145602e-35
-35.00:63051167601469893856e-35
-34.75:80959301752126006249e-35
-34.50:1039538011670221944e-33
-34.25:13347932285976030013e-34
-34.00:17139084315420129663e-34
-33.75:22007019879753666488e-34
-33.50:28257572871156112102e-34
-33.25:36283441780470446345e-34
-33.00:46588861451033973642e-34
-32.75:59821282237671354351e-34
-32.50:76812046852020949067e-34
-32.25:98628620465804520664e-34
-32.00:12664165549094175723e-33
-31.75:16261110446178189415e-33
-31.50:20879679116459335505e-33
-31.25:26810038677818032222e-33
-31.00:34424771084699764584e-33
-30.75:4420228103641172961e-32
-30.50:56756852326327224619e-33
-30.25:72877240958196924193e-33
-30.00:93576229688401746049e-33
-29.75:12015425731771785743e-32
-29.50:1542811203191887833e-31
-29.25:19810087980489795691e-32
-29.00:25436656473769229103e-32
-28.75:3266131342787447136e-31
-28.50:41937956583795444253e-32
-28.25:53849402177540356665e-32
-28.00:69144001069402030094e-32
-27.75:88782654784596584473e-32
-27.50:11399918530443553453e-31
-27.25:14637785141259089276e-31
-27.00:18795288165390832948e-31
-26.75:24133627718332140455e-31
-26.50:30988191387218254416e-31
-26.25:39789625358372400943e-31
-26.00:51090890280633247199e-31
-25.75:65602001681537786682e-31
-25.50:84234637544686474059e-31
-25.25:10815941557285692308e-30
-25.00:13887943864964020595e-30
-24.75:17832472908146389494e-30
-24.50:22897348456455528941e-30
-24.25:29400777392844724843e-30
-24.00:37751345442790977516e-30
-23.75:48473687062702555447e-30
-23.50:62241446229077832321e-30
-23.25:79919598929539319543e-30
-23.00:10261879631701890304e-29
-22.75:13176514270095466813e-29
-22.50:16918979226151303613e-29
-22.25:21724399350790169583e-29
-22.00:27894680928689248077e-29
-21.75:35817479302831807357e-29
-21.50:45990553786523167791e-29
-21.25:59053039989440397431e-29
-21.00:75825604279119067279e-29
-20.75:97362003130095654095e-29
-20.50:12501528663867426289e-28
-20.25:16052280551856116087e-28
-20.00:2061153622438557828e-27
-19.75:26465736389091170007e-28
-19.50:33982678194950712251e-28
-19.25:43634622529437014933e-28
-19.00:560279643753726754e-26
-18.75:71941330303253835055e-28
-18.50:92374496619705948979e-28
-18.25:11861120151343829833e-27
-18.00:15229979744712628436e-27
-17.75:195556810878504954e-25
-17.50:25109991557439818035e-27
-17.25:32241867372567333107e-27
-17.00:41399377187851666597e-27
-16.75:53157852544244215455e-27
-16.50:68256033763348697554e-27
-16.25:87642482194436362887e-27
-16.00:11253517471925911451e-26
-15.75:1444980246109244758e-25
-15.50:18553913626159782407e-26
-15.25:2382369667501817918e-25
-15.00:30590232050182578837e-26
-14.75:39278635454810390256e-26
-14.50:50434766256788807589e-26
-14.25:64759521758422092483e-26
-14.00:83152871910356788406e-26
-13.75:10677040100347826947e-25
-13.50:13709590863840843645e-25
-13.25:17603463121561692986e-25
-13.00:22603294069810543258e-25
-12.75:29023204086504038856e-25
-12.50:37266531720786709929e-25
-12.25:47851173921290090896e-25
-12.00:61442123533282097587e-25
-11.75:78893248272002232423e-25
-11.50:10130093598630710729e-24
-11.25:13007297654067620979e-24
-11.00:16701700790245659313e-24
-10.75:21445408316589163929e-24
-10.50:27536449349747157857e-24
-10.25:35357500850409982405e-24
-10.00:45399929762484851536e-24
-9.75:58294663730868807758e-24
-9.50:74851829887700591471e-24
-9.25:9611165206139469382e-23
-9.00:1234098040866795495e-22
-8.75:15846132511575125041e-23
-8.50:20346836901064417437e-23
-8.25:26125855730166753249e-23
-8.00:33546262790251183882e-23
-7.75:43074254057568753685e-23
-7.50:5530843701478335831e-22
-7.25:71017438884254906358e-23
-7.00:911881965554516208e-21
-6.75:1170879620791174401e-21
-6.50:15034391929775724474e-22
-6.25:19304541362277092422e-22
-6.00:2478752176666358423e-21
-5.75:3182780796509667068e-21
-5.50:40867714384640669935e-22
-5.25:52475183991813842765e-22
-5.00:67379469990854670966e-22
-4.75:86516952031206341771e-22
-4.50:11108996538242306496e-21
-4.25:14264233908999255273e-21
-4.00:18315638888734180294e-21
-3.75:23517745856009108236e-21
-3.50:3019738342231850074e-20
-3.25:38774207831722009887e-21
-3.00:49787068367863942979e-21
-2.75:63927861206707572702e-21
-2.50:8208499862389879517e-20
-2.25:10539922456186433678e-20
-2.00:13533528323661269189e-20
-1.75:17377394345044512668e-20
-1.50:22313016014842982893e-20
-1.25:28650479686019010032e-20
-1.00:3678794411714423216e-19
-0.75:47236655274101470714e-20
-0.50:6065306597126334236e-19
-0.25:77880078307140486825e-20
0.00:1e+0
0.25:12840254166877414841e-19
0.50:16487212707001281468e-19
0.75:21170000166126746685e-19
1.00:27182818284590452354e-19
1.25:34903429574618413761e-19
1.50:44816890703380648226e-19
1.75:57546026760057304369e-19
2.00:73890560989306502272e-19
2.25:94877358363585257206e-19
2.50:12182493960703473438e-18
2.75:1564263188418817161e-17
3.00:20085536923187667741e-18
3.25:25790339917193062089e-18
3.50:33115451958692313751e-18
3.75:42521082000062783056e-18
4.00:54598150033144239078e-18
4.25:70105412346687858102e-18
4.50:9001713130052181355e-17
4.75:11558428452718765813e-17
5.00:14841315910257660342e-17
5.25:19056626845862999618e-17
5.50:24469193226422038792e-17
5.75:31419066028569419814e-17
6.00:40342879349273512261e-17
6.25:51801282466834202594e-17
6.50:66514163304436184069e-17
6.75:85405876252615155278e-17
7.00:10966331584284585993e-16
7.25:1408104848204695575e-15
7.50:18080424144560632069e-16
7.75:23215724146110567464e-16
8.00:29809579870417282747e-16
8.25:38276258214399062273e-16
8.50:49147688402991343754e-16
8.75:6310688108089023997e-15
9.00:81030839275753840077e-16
9.25:10404565716560723288e-15
9.50:13359726829661872276e-15
9.75:17154228809290985045e-15
10.00:22026465794806716517e-15
10.25:2828254192033497909e-14
10.50:36315502674246637739e-15
10.75:46630028453524329213e-15
11.00:59874141715197818455e-15
11.25:76879919764677763445e-15
11.50:98715771010760497428e-15
11.75:12675355900574341904e-14
12.00:16275479141900392081e-14
12.25:20898128886971296151e-14
12.50:26833728652087445696e-14
12.75:34455189613782370094e-14
13.00:44241339200892050333e-14
13.25:56807004002249126779e-14
13.50:72941636984770133186e-14
13.75:93658915823255445599e-14
14.00:12026042841647767777e-13
14.25:15441744670851405697e-13
14.50:19827592635375687671e-13
14.75:25459132895553061663e-13
15.00:32690173724721106393e-13
15.25:41975013938479676712e-13
15.50:53896984762830123678e-13
15.75:69205098318305803181e-13
16.00:88861105205078726368e-13
16.25:11409991763828444531e-12
16.50:1465071942895351691e-11
16.75:18811896119537229518e-12
17.00:24154952753575298215e-12
17.25:31015573274482230832e-12
17.50:39824784397576225022e-12
17.75:5113603538059727805e-11
18.00:65659969137330511139e-12
18.25:84309069231265055313e-12
18.50:10825498775023075725e-11
18.75:13900215575451639811e-11
19.00:17848230096318726084e-11
19.25:22917581086564340584e-11
19.50:29426756604150880657e-11
19.75:3778470341041358312e-10
20.00:48516519540979027797e-11
20.25:62296444219844548365e-11
20.50:7999021774755054067e-10
20.75:10270947267424175703e-10
21.00:13188157344832146972e-10
21.25:16933929230041595871e-10
21.50:21743595535764885455e-10
21.75:27919329318100222587e-10
22.00:35849128461315915617e-10
22.25:46031192110433541268e-10
22.50:59105220630232906143e-10
22.75:7589260554815570164e-9
23.00:97448034462489026e-7
23.25:12512575305609886385e-9
23.50:16066464720622478609e-9
23.75:20629749057596176166e-9
24.00:26489122129843472294e-9
24.25:34012706080464738693e-9
24.50:4367317909764641453e-8
24.75:56077471988933799045e-9
25.00:72004899337385872524e-9
25.25:92456120875245775651e-9
25.50:11871600913216965097e-8
25.75:15243437309343985937e-8
26.00:19572960942883876427e-8
26.25:25132179330499358875e-8
26.50:32270357037115483078e-8
26.75:4143595864124439884e-7
27.00:53204824060179861668e-8
27.25:68316346383670420573e-8
27.50:87719925131876492831e-8
27.75:11263461341927520006e-7
28.00:14462570642914751737e-7
28.25:18570308296144511349e-7
28.50:23844747847976778768e-7
28.75:30617262291312510388e-7
29.00:39313342971440420744e-7
29.25:50479331590291879204e-7
29.50:64816744779343202179e-7
29.75:83226347723639147632e-7
30.00:10686474581524462147e-6
30.25:13721704977464905311e-6
30.50:17619017951355631412e-6
30.75:22623266866618211942e-6
31.00:29048849665247425231e-6
31.25:37299461295718884905e-6
31.50:47893456332463727075e-6
31.75:61496415223907888198e-6
32.00:78962960182680695161e-6
32.25:10139044785146411902e-5
32.50:13018791205063293871e-5
32.75:16716458801852100004e-5
33.00:21464357978591606462e-5
33.25:27560781197395935993e-5
33.50:35388743561225987393e-5
33.75:45440046197258826949e-5
34.00:5834617425274548814e-4
34.25:74917970707017099022e-5
34.50:96196578554477641049e-5
34.75:12351885186234820941e-4
35.00:15860134523134307281e-4
35.25:20364815839791162985e-4
35.50:26148941144456966074e-4
35.75:33575905048954583547e-4
36.00:43112315471151952271e-4
36.25:55357308837219249338e-4
36.50:71080191546422440649e-4
36.75:91268772568639554088e-4
37.00:11719142372802611309e-3
37.25:15047676668460840439e-3
37.50:19321599304402836208e-3
37.75:24809424597909427774e-3
38.00:3185593175711375622e-2
38.25:40903826048404247573e-3
38.50:52521552285925158157e-3
38.75:67439008059022052463e-3
39.00:86593400423993746954e-3
39.25:11118812706182702103e-2
39.50:14276838118129198592e-2
39.75:18331823013614275199e-2
40.00:23538526683701998541e-2
40.25:30224066533255980371e-2
40.50:38808469624362032402e-2
40.75:49831061380435016884e-2
41.00:63984349353005494922e-2
41.25:82157530839486902849e-2
41.50:10549235777020814185e-1
41.75:13545486864326381243e-1
42.00:17392749415205010474e-1
42.25:22332732315204085594e-1
42.50:28675795916805715596e-1
42.75:368204508009290948e+1
43.00:47278394682293465615e-1
43.25:60706660432259368343e-1
43.50:77948894957253063996e-1
43.75:10008836232783585641e+0
44.00:12851600114359308276e+0
44.25:16501781191944436918e+0
44.50:21188706471076390949e+0
44.75:2720683765559810729e+1
45.00:34934271057485095348e+0
45.25:4485649195126980689e+1
45.50:57596875768879535865e+0
45.75:73955852409047626713e+0
46.00:94961194206024488745e+0
46.25:1219325869595561365e+2
46.50:15656454077855834166e+1
46.75:20103284971171326815e+1
47.00:25813128861900673962e+1
47.25:33144713542916378987e+1
47.50:42558654617939031863e+1
47.75:54646394229468838738e+1
48.00:70167359120976317387e+1
48.25:90096672533190013954e+1
48.50:11568641749160830075e+2
48.75:14854430042477437334e+2
49.00:19073465724950996905e+2
49.25:24490814775159559005e+2
49.50:31446828646696548517e+2
49.75:40378527256582541339e+2
50.00:51847055285870724641e+2
50.25:66572936767472526876e+2
50.50:85481342872980576923e+2
50.75:10976021690150658605e+3
51.00:14093490824269387964e+3
51.25:18096400428217362072e+3
51.50:2323623810039002176e+4
51.75:29835920309108872329e+3
52.00:38310080007165768493e+3
52.25:49191116444541740149e+3
52.50:63162643790037920251e+3
52.75:81102440011602827585e+3
53.00:10413759433029087797e+4
53.25:13371531795281072966e+4
53.50:17169386685189163673e+4
53.75:22045928892722976404e+4
54.00:28307533032746939004e+4
54.25:36347591897774894756e+4
54.50:46671231832136385508e+4
54.75:59927047900589106814e+4
55.00:76947852651420171382e+4
55.25:98802998563966718928e+4
55.50:12686556140109568975e+5
55.75:16289860534136614536e+5
56.00:20916594960129961539e+5
56.25:26857439559369587339e+5
56.50:34485635021385366422e+5
56.75:44280431878075715825e+5
57.00:56857199993359322226e+5
57.25:73006089913171456063e+5
57.50:93741675021502699374e+5
57.75:12036669333049185121e+6
58.00:15455389355901039304e+6
58.25:19845112757782116459e+6
58.50:25481629178026396623e+6
58.75:32719059523197855451e+6
59.00:4201210403790514255e+7
59.25:53944609393199897211e+6
59.50:69266249554160951309e+6
59.75:889396249461786033e+8
60.00:11420073898156842837e+7
60.25:14663665145685640327e+7
60.50:18828518748858515756e+7
60.75:24176296632116008646e+7
61.00:31042979357019199087e+7
61.25:39859974504125534321e+7
61.50:51181220371822540943e+7
61.75:65717987814516561268e+7
62.00:84383566687414544891e+7
62.25:10835064437740528235e+8
62.50:13912498129508311164e+8
62.75:17864001207909333228e+8
63.00:22937831594696098791e+8
63.25:29452758771292899986e+8
63.50:3781809085391289879e+9
63.75:48559389867030375025e+8
64.00:62351490808116168829e+8
64.25:8006089896599324668e+9
64.50:10280022915520464989e+9
64.75:13199810707660696131e+9
65.00:16948892444103337141e+9
65.25:21762808682935500663e+9
65.50:27943999487401854681e+9
65.75:35880805585733200894e+9
66.00:46071866343312915427e+9
66.25:59157447379054298782e+9
66.50:75959666021073336335e+9
66.75:97534141814170368893e+9
67.00:12523631708422137805e+10
67.25:16080661422850547256e+10
67.50:20647977984090163798e+10
67.75:26512528534780684974e+10
68.00:34042760499317405214e+10
68.25:43711769735337017573e+10
68.50:56127023348574721279e+10
68.75:72068524542596251832e+10
69.00:92537817255877876002e+10
69.25:11882090936135266395e+11
69.50:1525690676539272172e+12
69.75:19590256066799411611e+11
70.00:25154386709191670063e+11
70.25:32298871875794420422e+11
70.50:41472572418860905091e+11
70.75:53251837081240408409e+11
71.00:68376712297627438668e+11
71.25:8779743649969888934e+12
71.50:11273413998564138954e+12
71.75:14475350106999736396e+12
72.00:18586717452841279803e+12
72.25:23865817622241841327e+12
72.50:30644316416992723992e+12
72.75:39348081156440079544e+12
73.00:50523936302761041946e+12
73.25:64874018363857655767e+12
73.50:83299888461860519478e+12
73.75:10695917399228284257e+13
74.00:13733829795401761878e+13
74.25:17634586525759266668e+13
74.50:22643257311854073807e+13
74.75:2907451790502117624e+14
75.00:37332419967990016403e+13
75.25:47935776105360141408e+13
75.50:615507548879353372e+15
75.75:79032733692426212242e+13
76.00:10148003881138887278e+14
76.25:13030294912028177541e+14
76.50:16731229853981138431e+14
76.75:21483324384956511381e+14
77.00:27585134545231702063e+14
77.25:35420013878828548428e+14
77.50:45480198079848413399e+14
77.75:58397730290518379155e+14
78.00:74984169969901204347e+14
78.25:96281580090586825727e+14
78.50:12362799599516990337e+15
78.75:1587414890719684706e+16
79.00:20382810665126687668e+15
79.25:26172046957556636282e+15
79.50:33605573500247796662e+15
79.75:43150410516686200204e+15
80.00:55406223843935100526e+15
80.25:71142999658303045143e+15
80.50:9134941978066841756e+16
80.75:11729497679805617914e+16
81.00:15060973145850305484e+16
81.25:19338672319323323195e+16
81.50:24831346783006822105e+16
81.75:31884080399968143772e+16
82.00:40939969621274546967e+16
82.25:52567961552140528084e+16
82.50:67498598736412415156e+16
82.75:86669916368360612187e+16
83.00:11128637547917594121e+17
83.25:14289453464631734428e+17
83.50:18348021439163854018e+17
83.75:23559325873817981842e+17
84.00:30250773222011423383e+17
84.25:38842761691519589936e+17
84.50:49875093266256083711e+17
84.75:6404088741354443731e+18
85.00:82230127146229135103e+17
85.25:10558557327322282771e+18
85.50:13557455971836400187e+18
85.75:17408118053462942925e+18
86.00:22352466037347150474e+18
86.25:2870113451760326459e+19
86.50:36852986208376451985e+18
86.75:47320170972398163873e+18
87.00:60760302250568721495e+18
87.25:78017772415359619297e+18
87.50:10017680273468151854e+19
87.75:12862956087384511746e+19
88.00:16516362549940018555e+19
88.25:21207429305352540792e+19
88.50:27230878250681116121e+19
88.75:34965139792603977031e+19
89.00:44896128191743452463e+19
89.25:57647769709069644146e+19
89.50:7402120152180711149e+20
89.75:95045104127765660391e+19
90.00:1220403294317840802e+21
90.25:15670288485135579448e+20
90.50:20121048701743329676e+20
90.75:25835937943450318708e+20
91.00:33174000983357426258e+20
91.25:42596260435855064996e+20
91.50:54694681055488356515e+20
91.75:70229360632876557178e+20
92.00:90176284050342989314e+20
92.25:11578864070309379322e+21
92.50:14867555762649719192e+21
92.75:19090319483264537812e+21
93.00:24512455429200857856e+21
93.25:31474615796519322531e+21
93.50:40414206663212293443e+21
93.75:51892868550835663453e+21
94.00:66631762164108958342e+21
94.25:85556876177408492502e+21
94.50:10985720358419844238e+22
94.75:14105944160835045224e+22
95.00:18112390828890232822e+22
95.25:23256770181277008566e+22
95.50:29862284022825251975e+22
95.75:38343931685655879194e+22
96.00:49234582860120583998e+22
96.25:63218455772413467762e+22
96.50:8117410401552875886e+23
96.75:10422961273279338382e+23
97.00:13383347192042695005e+23
97.25:17184557954939336403e+23
97.50:2206540918868562407e+24
97.75:28332546227887578202e+23
98.00:36379709476088045793e+23
98.25:46712471619012930437e+23
98.50:5998000083511737607e+24
98.75:77015845565242671006e+23
99.00:98890303193469467706e+23
99.25:12697766276437172563e+24
99.50:16304254634105792122e+24
99.75:20935077350340729796e+24
100.00:26881171418161354484e+24
100.25:34516107331259239871e+24
100.50:4431955909845895416e+25
100.75:56907440338815742922e+24
101.00:73070599793680672726e+24
101.25:93824507347704022713e+24
101.50:12047305214265772033e+25
101.75:15469046097712008637e+25
102.00:19862648361376543259e+25
102.25:25504145338738601552e+25
102.50:32747970845838552539e+25
102.75:42049226911005857362e+25
103.00:53992276105801688698e+25
103.25:69327454824671601441e+25
103.50:89018214069149526772e+25
103.75:11430164941293829253e+26
104.00:14676622301554423285e+26
104.25:18845156066322017808e+26
104.50:24197659370604648107e+26
104.75:31070409656208665623e+26
105.00:39895195705472158508e+26
105.25:51226445289557882906e+26
105.50:65776057758356352562e+26
105.75:84458129971250466654e+26
106.00:10844638552900230813e+27
106.25:13924791536715664689e+27
106.50:17879786255221267423e+27
106.75:22958099996648240809e+27
107.00:29478783914555093774e+27
107.25:37851507799334495336e+27
107.50:48602298074299772045e+27
107.75:62406586036834580316e+27
108.00:80131642640005911411e+27
108.25:10289106583070678347e+28
108.50:13211474367671911753e+28
108.75:16963868880009342429e+28
109.00:21782038807290206356e+28
109.25:27968691455839362744e+28
109.50:35912510700795012746e+28
109.75:46112576516891291178e+28
110.00:5920972027664670299e+29
110.25:76026785750185898786e+28
110.50:97620325252312095227e+28
110.75:1253469788092928904e+30
111.00:16094870669615180549e+29
111.25:20666223018087941005e+29
111.50:26535955622162162862e+29
111.75:34072841474954187489e+29
112.00:43750394472613410735e+29
112.25:56176618492950496549e+29
112.50:72132205968519045371e+29
112.75:92619585825333660519e+29
113.00:1189259022828200882e+31
113.25:15270388123366368993e+30
113.50:19607566473089040657e+30
113.75:251766137108407451e+32
114.00:32327411910848593114e+30
114.25:4150921854926362193e+31
114.50:53298891644100750211e+30
114.75:68437131552311248574e+30
115.00:87875016358370231131e+30
115.25:11283375449599843527e+31
115.50:14488140863316671496e+31
115.75:18603141109050883756e+31
116.00:23886906014249914255e+31
116.25:30671394448328164271e+31
116.50:39382850036908651969e+31
116.75:5056858042899246693e+32
117.00:64931342556644621362e+31
117.25:83373494182390091402e+31
117.50:10705368560825642761e+32
117.75:13745965327109993311e+32
118.00:17650168856917655833e+32
118.25:22663265421112690832e+32
118.50:29100208825849105824e+32
118.75:37365407763311190463e+32
119.00:4797813327299302186e+33
119.25:61605142567754859039e+32
119.50:7910256885566915324e+33
119.75:10156970893597134655e+33
120.00:13041808783936322797e+33
