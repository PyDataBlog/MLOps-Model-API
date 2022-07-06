#ExStart:1
use lib 'lib';
use strict;
use warnings;
use utf8;
use File::Slurp; # From CPAN
use JSON;

use AsposeStorageCloud::StorageApi;
use AsposeStorageCloud::ApiClient;
use AsposeStorageCloud::Configuration;
use AsposeWordsCloud::WordsApi;
use AsposeWordsCloud::ApiClient;
use AsposeWordsCloud::Configuration;

my $configFile = '../Config/config.json';
my $configPropsText = read_file($configFile);
my $configProps = decode_json($configPropsText);
my $data_path = '../../../Data/';
my $out_path = $configProps->{'out_folder'};

$AsposeWordsCloud::Configuration::app_sid = $configProps->{'app_sid'};
$AsposeWordsCloud::Configuration::api_key = $configProps->{'api_key'};
$AsposeWordsCloud::Configuration::debug = 1;
$AsposeStorageCloud::Configuration::app_sid = $configProps->{'app_sid'};
$AsposeStorageCloud::Configuration::api_key = $configProps->{'api_key'};

# Instantiate Aspose.Storage and Aspose.Words API SDK
my $storageApi = AsposeStorageCloud::StorageApi->new();
my $wordsApi = AsposeWordsCloud::WordsApi->new();

# Set input file name
my $name = "SampleWordDocument.docx";
my $objectIndex = 1;

# Upload file to aspose cloud storage
my $response = $storageApi->PutCreate(Path => $name, file => $data_path.$name);
# Invoke Aspose.Words Cloud SDK API to get drawing object by index present in a word document
$response = $wordsApi->GetDocumentDrawingObjectImageData(name=> $name, objectIndex=>$objectIndex);

if($response->{'Status'} eq 'OK'){
	# Download output document from response
	my $destfilename = $out_path. "DrawingObject_" . $objectIndex . ".png";
	write_file($destfilename, { binmode => ":raw" }, $response->{'Content'});
	}
#ExEnd:1