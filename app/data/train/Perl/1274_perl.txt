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
my $name =  'SampleInvoiceTemplate.doc';
my $filedataname = "SampleInvoiceTemplateData.txt";
my $destfilename = "updated-" . $name;

# Upload file to aspose cloud storage 
my $response = $storageApi->PutCreate(Path => $name, file => $data_path.$name);
# Invoke Aspose.Words Cloud SDK API to execute mail merge and populate a word document from XML data      
$response = $wordsApi->PostDocumentExecuteMailMerge(name=> $name, file=>$data_path.$filedataname, withRegions=>'True');

if($response->{'Status'} eq 'OK'){
	print "\nmail merge template has been executed successfully.";
	# Download updated document from storage server
	$response = $storageApi->GetDownload(Path => $destfilename); 
	my $output_file = $out_path. $destfilename;	
	write_file($output_file, { binmode => ":raw" }, $response->{'Content'});
	}
#ExEnd:1