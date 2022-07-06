#!/usr/bin/perl -w

# ***************************************************************************************
# FUNCTION: Upload, check and process cytogenetics Sample Report files to produce a 
#			tab separated values (tsv) file in a user defined format (see example file).
#
# ARGUMENTS: Cytogenetics Sample Report file (xml format) and the respective 
# 			 Interval Based Report (IBR) file (xls extension; tab delimited format)
#
# RETURNS: Uploads the input Sample Report (xml) and IBR (xls) files.
# 			Produces an excel workbook as temporary file in folder /tmp/
#			Converts excel workbook into tab separated values file (txt) in folder /var/www/Upload
#			Deletes uploaded Sample Report (xml) and IBR (xls) files.
#			Returns html file to browser with acknowledgement of processing and prompts
#			the user to restart with different files.
#
# NOTES: The web page cytoReport.html is used to upload the two input files and direct
#		 filenames as arguments to this file using the perl CGI module.
#		 Secure mode -T was disabled so that system commands could be passed to convert 
# 		 the excel workbook to csv (ssconvert), change comma separators to tab separators
#		and change line separators from linefeed to "|" (vertical tab symbol).
#        	The input files and the csv file are deleted after processing.
#		  
# ***************************************************************************************

use strict;
use warnings;

use CGI qw(:standard);
use CGI::Carp qw (warningsToBrowser fatalsToBrowser);
use File::Basename;

use XML::LibXML;
use Spreadsheet::WriteExcel;
use Text::CSV;

$CGI::POST_MAX = 1024 * 5000; # limit the allowable size of an uploaded file to 5MB

######################################################################################
sub error {
# FUNCTION: pass error messages to browser
# REQUIRES: query object ($q) and string with error message ($reason) as arguments
# USAGE: error($q, $error);

	my( $q, $reason ) = @_;
    
	print $q->header( "text/html" ),
		$q->start_html( "Error" ),
		$q->h1( "Error" ),
		$q->p( "Your upload was not processed because the following error occured:\n" ),
		$q->p( $q->i( $reason ) ),
		$q->p( "Please, try again.\n" ),
		$q->end_html;
    exit;
}## end sub error
#######################################################################################


#######################################################################################
sub wash_filename {
# FUNCTION: clean file name from unsafe characters and spaces
# REQUIRES: file name from file handle object $file
# USAGE: wash_filename ($file);

    my $filename = shift();
	
	# create a list of “safe” characters for filenames
	my $safe_filename_characters = "a-zA-Z0-9_.-";
	
	# make the filename safe:
	# use the fileparse routine in the File::Basename module to split the filename into its
	# leading path (if any), the filename itself, and the file extension
	# regex '..*' means a literal period (.) followed by zero or more characters
	my ( $name, $path, $extension ) = fileparse ( $filename, '..*' );
	$filename = $name . $extension;
	
	# clean up the filename to remove any characters that aren’t safe
	# convert spaces to underscore characters
	$filename =~ tr/ /_/;
	$filename =~ s/[^$safe_filename_characters]//g;

	#use regular expression matching to extract the safe characters
	if ( $filename =~ /^([$safe_filename_characters]+)$/ ){
		$filename = $1;
	}else{ # extra caution: filename should not have any unsafe characters at this point
		die "Filenames contains invalid characters"; #safeguard NOT WORKING
	}
	
    return $filename;
} ## end sub wash_filename
##########################################################################################

##########################################################################################
# create a CGI object to read in the names of the files and the name of the user
# from the html page

my $query = new CGI ();

my @files = $query->param('multiple_files');

if (scalar(@files) < 2){
	error( $query, "Expected two files but only one file was selected." );
}

#######################################################################


###########################################################################################
# Upload files to server
#################

my $upload_dir = "/var/www/Upload/"; # a location on the server to store the uploaded files
# Make sure the directory can be read and written to by the script
# on a shared UNIX server

# initialise clean file names
my $xml_file = '';
my $xls_file = '';

foreach my $file (@files) {

	# test if $filename exists and report a problem
	if ( !$file ){
		error( $query, "There was a problem uploading one of the files.");
	}
	
	# test file extension and exit if not xml or xls
	# use regex to check file extensions
	unless (($file =~ /\.xml$/i) or ($file =~ /\.xls$/i)){ 
		error ( $query, "Unexpected file extension." );
		exit;
	}
}

#upload files with safe names
foreach my $file (@files) {
	
	my $safeFilename = wash_filename ($file);

	my $serverFullFilename = $upload_dir.$safeFilename;
	
	# read file and save it to the upload folder
	# if there’s an error writing the file, the 'die' function stops the script running
	# and reports the error message (stored in the special variable $!)
	open ( UPLOADFILE, ">$serverFullFilename" ) or die "$!";

	# the binmode function tells Perl to write the file in binary mode, rather than in text mode
	# to prevent the uploaded file from being corrupted on non-UNIX servers (such as Windows machines)
	binmode UPLOADFILE;
	
	# use the value returned by param (in this case $file)
	# as a file handle in order to read from the file
	while ( <$file> ){
		
		if ($file =~ /\.xml$/i) { # regex to match file extension to xml
			$xml_file = $safeFilename;
			print UPLOADFILE;		
		} 
		
		elsif ($file =~ /\.xls$/i){ # regex to match file extension to xls
			$xls_file = $safeFilename;
			print UPLOADFILE;
		
		}
		
	}
	
	close UPLOADFILE;

}

############################################################################################


############################################################################################
# File location and file name variables
###############################

# INPUT and OUTPUT file names and paths

my $input_dir = $upload_dir;                                         #$input_dir = "/var/www/Upload/";

# INPUT IntervalBasedReport (ibr) file (.xls)
my $ibrpath = $input_dir.$xls_file;

# INPUT .xml file 
my $xmlpath = $input_dir.$xml_file;

# OUTPUT to excel workbook and csv file
# name files according to xml file name (remove .xml extension and add other extension)
my @xml_namepart = split /[.]/, $xml_file;
my $reportname = $xml_namepart[0];

# output xls file name and path
my $xlsdir = "/tmp/";
my $outxls = $reportname."_processed.xls";
my $xlspath = $xlsdir."$outxls";

# output csvfile name and path
my $outdir = "/var/www/Upload/";
my $outcsv = $reportname."_processed.csv";
my $csvpath = $outdir."$outcsv";

# output tab separated values file name (extension txt) and path
my $outtxt = $reportname."_processed.txt";
my $txtpath = $outdir."$outtxt";

############################################################################################


############################################################################################
# Create Document Object Model (DOM) and find nodes in XML file;
# Populate data structures %sample_info and %aberrations_info
#

my $parser = XML::LibXML->new();
my $xmldoc = $parser->parse_file($xmlpath);

#check that xml file was parsed
unless ($xmldoc){
    error ( $query, "Could not open xml file." ); #print "ERROR: Could not open xml file : $!\n";
    exit;
}

# Check if it is the right type of xml file
my $header = $xmldoc->findnodes('/report/header/text');
my $str_header = sprintf("%s", $header); 	# use sprintf to convert $header to string
unless ( $str_header =~ /OXFORD GENETICS LABORATORIES-CYTOGENETICS/){
	system("rm '$ibrpath' '$xmlpath' ");
	error( $query, "The xml file had unexpected content. Please upload a signed off xml report.");
	exit;
}

# initialise hashes
my %sample_info = ();
my %aberrations_info = ();

######################
#%sample_info

#SampleID
my $sampleID = $xmldoc->findnodes('/report/header/headerattribValue');
$sample_info{'SampleID'} = sprintf("%s", $sampleID); #use sprintf to convert to string 

#Analysis method
my $method = $xmldoc->findnodes('/report/footer/analysisMethod');
$sample_info{'AnalysisMethod'} = sprintf("%s", $method);

#Polarity
my $pol = $xmldoc->findnodes("/report/sections/section/
        tname[text( )='sampleInformation']/../
        sampleInformation/data/pair/
		name[text( )='Polarity']/../value/text( )");

$sample_info{'Polarity'} = sprintf("%s", $pol);

#ScanDate
my $date = $xmldoc->findnodes("/report/sections/section/
        tname[text( )='sampleInformation']/../
        sampleInformation/data/pair/
		name[text( )='Scan_Date']/../value/text( )");

$sample_info{'ScanDate'} = sprintf("%s", $date);

#DLRSpread
my $dlrs = $xmldoc->findnodes("/report/sections/section/
        tname[text( )='sampleInformation']/../
        sampleInformation/QcMetricTables/QcMetricTable/QcMetric/
		QcMetricName[text( )='DerivativeLR_Spread']/../QcMetricValue/text( )");

$sample_info{'DLRSpread'} = sprintf("%s", $dlrs);


######################
#%aberrations_info

my @aberrations = $xmldoc->findnodes("/report/sections/section/
		tname[text( )='tableView']/../
		sample/chromosomes/chromosome/aberrations/aberration");


my $aberration_count = 1;

my @aberration_char = ('chr', 'location', 'size', 'cytoband', 'probes', 'type', 'amplification', 'deletion', 'classification');

#for each aberration entry, find value and add pair aberration_char = value to %aberrations_info
foreach my $aberration (@aberrations){
	foreach my $char (@aberration_char){
		
		my $char_value = $aberration->findvalue('./'.$char);
		
		$aberrations_info{$aberration_count}{$char} = $char_value;
	}
	
	$aberration_count++;
	
}

##########################################################################################


##########################################################################################
# OPEN interval based report (ibr) file 
# Get GenomeBuild and gene list for each variant into %lookup_genes 

unless (open (FILE, $ibrpath)){
    error ( $query, "Could not open Interval Based Report file." ); 
    #print "ERROR: Could not open Interval Based Report file: $!\n";
    exit;
}
 
 my $fileline;
 my @line_data;

while ($fileline = <FILE>){     # read in each line in the file

	chomp($fileline);              	# remove the ending to new line
	#print "$fileline";
	
	push @line_data, $fileline; # generate array of lines     
	
}

close FILE;

# Get lines that contain "sample info: value"
my @info_lines = grep { /.*:/ } @line_data; #match any character except new line(.), 0 or more times (*), followed by a : (:)

# Get GenomeBuild and add it to %sample_info key = value pairs
my @build = split (/: /, $info_lines[0]);

$sample_info{'GenomeBuild'} = sprintf("%s", $build[1]);

# Get aberrations
my @aberration_lines = grep { /^[1-9]+/ } @line_data; #match any number ([1-9]+) at begining of line (^)

# initialise hash to store gene lists (pair variant=>genes) 
my %lookup_genes = ();

foreach my $aberration (@aberration_lines){

	my @variant_char = split (/\t/, $aberration);

	my $chr = $variant_char[1];
	$chr =~ s/[chr]//g; # remove 'chr' from $chr
	
	my $cytoband = $variant_char[2];
	$cytoband =~ s/\s[-]\s//g; #?remove ' - ' from cytoband string
		
	my $start = $variant_char[3];
	$start =~ s/[,]//g; # remove ',' from $start
	my $stop = $variant_char[4];
	$stop =~ s/[,]//g; # remove ',' from $stop
	my $location = $start."-".$stop;
	
	my $variant = $chr.$cytoband."(".$location.")"; 
	#corresponds to variant description in xls and is used to get genes
	
	my $genes = $variant_char[11];
	
	# Add pair variant=>genes  to hash %variant2genes
	$lookup_genes{$variant} = $genes;

}

########################################################################################


########################################################################################
# Create Excel workbook

# Create a new workbook
my $workbook  = Spreadsheet::WriteExcel->new($xlspath);

#check the return value of new before proceeding
die "Problems creating new Excel file: $!" unless defined $workbook;

#Format headers in worksheets
my %format = ();
$format{'header'} = $workbook->add_format();
$format{'header'}->set_bold();

# Add worksheet and respective headers
my $worksheet = $workbook->add_worksheet("Report");

my @sample_headers = ("SampleID", "AnalysisMethod", "DLRSpread", "Polarity", "ScanDate", "GenomeBuild");
my @report_headers = ("Variant", "Chromosome", "Start", "Stop", "Cytoband", "#Probes", "Size", "Type", "Amp/Del", "Classification", "Genes", "Notes");

$worksheet->write_col(0, 0, \@sample_headers, $format{'header'});
$worksheet->write_row(7, 0, \@report_headers, $format{'header'});

###########################################################################################


############################################################################################
# Write to workbook
#

#Sample Information
for (my $i=0; $i <= (scalar(@sample_headers))-1; $i++) {
	#line $i, column 1; count starts at zero)
	$worksheet->write($i, 1, $sample_info{$sample_headers[$i]}); #get values from sample_info hash
}

# Aberration Information
# line count starts at 8; column count starts at 0)
# reminder: my @report_headers = ("Variant", "Chromosome", "Start", "Stop", "Cytoband", "#Probes", "Size", "Type", "Amp/Del", "Classification", "Genes", "Notes");

my $line = 8; # first destination line for aberration details in worksheet

foreach my $key (sort keys (%aberrations_info)){
	my $chr_number; # initialise variable to register chromosome coordinate
	
	my $variant; # initialise variable for variant name; needed to retrieve genes from %lookup_genes
	my $ISCN_variant; #initialise variant ICSN nomenclature
	my $xvariant; # initialise variable to add amp/del to variant ISCN numenclature
	
	my $genes; # initialise variable to add genes to column 10
	my $interval2note; # initialise variable for interval, to retrieve interval note to column 11

	
	foreach my $header (sort keys %{$aberrations_info{$key}}){
		# print $header, " = ", $aberrations_info{$key}{$header}, "\n";

		if ($header eq 'chr'){
			$chr_number = $aberrations_info{$key}{$header};
			
			# column 1
			$worksheet->write($line, 1, $chr_number);
						
			# add $chr_number to $interval2note string
			$interval2note.= $chr_number.":";
			
			# add $chr_number to $variant and $ISCN_variant
			$chr_number =~ s/[chr]//g; # remove 'chr' from $chr_number
			$variant.= $chr_number;
			$ISCN_variant.= $chr_number;
			
		}

		if ($header eq 'cytoband'){
			# get cytoband and remove ' - ' from $cytoband for ISCN nomenclature
			my $cytoband = $aberrations_info{$key}{$header};
			$cytoband =~ s/\s[-]\s//g;
			
			# column 4
			$worksheet->write($line, 4, $cytoband);
						
			# add $cytoband to $variant string
			$variant .= $cytoband;
			$ISCN_variant.= $cytoband;
			
		}
		
		if ($header eq 'location'){
			# get start and stop and add to worksheet at columns 2 and 3, respectively
			my $location = $aberrations_info{$key}{$header};
			# variant nummenclature used to retrieve genes in %lookup_genes
			$variant .= "(".$location.")";
			#print "variant: ", $variant, "\n";

			my @start_stop = split(/-/, $location);
			
			my $start = $start_stop[0];
			$worksheet->write($line, 2, $start);
			
			my $stop = $start_stop[1];
			$worksheet->write($line, 3, $stop);
			
			# variant nomenclature with start_stop notation
			my $underscore_location = $start."_".$stop;
			# add $underscore_location to $ISCN_variant string
			$ISCN_variant .= "(".$underscore_location.")";
			
			# add $location to $interval2note string
			$interval2note .= $location;
			#print "interval2note: ", $interval2note, "\n";
		}
		
		if ($header eq 'probes'){
			# column 5
			$worksheet->write($line, 5, $aberrations_info{$key}{$header});
		
		}
		
		if ($header eq 'size'){
		
			my $size = $aberrations_info{$key}{$header};
			# to remove ',' from $size
			$size =~ s/[,]//g; 
			
			# column 6
			$worksheet->write($line, 6, $size);
		}

		if ($header eq 'type'){
			# column 7
			$worksheet->write($line, 7, $aberrations_info{$key}{$header});
			
			if ($aberrations_info{$key}{$header} eq 'Gain'){
				my $amplification = $aberrations_info{$key}{'amplification'};
				
				# column 8
				$worksheet->write($line, 8, $amplification);
				
				# add amplification times to $ISCN_variant

				if ($amplification >= 0 and $amplification < 0.40){
					$xvariant = $ISCN_variant."x2";
				}
				
				if ($amplification >= 0.40 and $amplification < 0.75){
					$xvariant = $ISCN_variant."x3";
				}
				
				if ($amplification >= 0.75){
					$xvariant = $ISCN_variant."x4";
				}
				
			}elsif ($aberrations_info{$key}{$header} eq 'Loss'){
				my $deletion = $aberrations_info{$key}{'deletion'};
				
				# column 8
				$worksheet->write($line, 8, $deletion);
				
				# add deletion times to $ISCN_variant

				if ($deletion > -0.70 and $deletion <= 0){
					$xvariant = $ISCN_variant."x2";
				}
				
				if ($deletion > -2.00 and $deletion <= -0.70){
					$xvariant = $ISCN_variant."x1";
				}
				
				if ($deletion <= -2.00){
					$xvariant = $ISCN_variant."x0";
				}
			
			}
		}
		
		if ($header eq 'classification'){
			# column 9
			$worksheet->write($line, 9, $aberrations_info{$key}{$header});
		}	
		
	}
	
	# variant name in column 0
	unless ($chr_number eq 'X' | $chr_number eq 'Y'){
		$worksheet->write($line, 0, $xvariant);
	}else{
		$worksheet->write($line, 0, $ISCN_variant);
	}
	
	# variant genes in column 10
	if (exists ($lookup_genes{$variant})){
		$genes = $lookup_genes{$variant};
		$worksheet->write($line, 10, $genes);
	}
	
	# get notes for $interval
	my $notes = $xmldoc->findnodes("/report/notes/intervalsNotes/intervalNote/
		interval[text( )= '$interval2note' ]/../note/noteText");

	my $separator = '; ';
	$separator =~  s/[ ]/\n/g; #regex to substitute space in $separator by newline - results in each note starting in a new line
	my $separatedNotes = $notes->to_literal_delimited($separator);

	#regex to remove the double hyphen '--' found when word coincides with a line break
	$separatedNotes =~ s/-\n-//g;

	#regex to replace 'en dash' (looks longer than a minus sign; is causing parsing problems) by an 'hyphen' (-)
	$separatedNotes =~ s/\x{2013}/-/g;
				
	#column 11
	$worksheet->write($line, 11, $separatedNotes);
	
	#line counter increase
	$line++;
}

$workbook->close();

###########################################################################################

#######################################################################
# Use system tools to convert Excel workbook to txt file
###################


system("ssconvert '$xlspath' '$csvpath'");


###########################################################################################
#Convert csv file to file with tab separated values and use | as end of line (eol)
############################

my $csv = Text::CSV->new ({ binary => 1 });
my $txt = Text::CSV->new ({ binary => 1, sep_char => "\t", eol => "|"});

open my $infh,  "<:encoding(utf8)", "$csvpath";
open my $outfh, ">:encoding(utf8)", "$txtpath";

while (my $row = $csv->getline ($infh)) {
    $txt->print ($outfh, $row);
    }

##########################################################################################
# Use system tools to delete input files and the csv file
###################


system("rm '$ibrpath' '$xmlpath' '$csvpath' ");


##########################################################################################

##########################################################################################
#Thank you note - acknowledge upload of files and processing
###########################

print $query->header ( );
print <<END_HTML;
<!DOCTYPE html PUBLIC "-//W3C//DTD XHTML 1.0 Strict//EN" "DTD/xhtml1-strict.dtd">
<html xmlns="http://www.w3.org/1999/xhtml" xml:lang="en" lang="en">
<head>
<meta http-equiv="Content-Type" content="text/html; charset=utf-8" />
<title>Thanks!</title>
<style>
body {
    background-color: lightblue;
}

h1 {
    color: white;
    text-align: center;
    font-size: 44px;
}

p {
    font-size: 20px;
    text-align: center;
}

div {
    background-color: white;
    padding: 10px;
    margin: auto;
    width: 50%;
    text-align: center;
}

a:link {
    background-color: orange;
    border: none;
    text-decoration: none;
    padding: 16px 32px;
    font-size: 20px;
    font-weight: bold;
    cursor: pointer;
}

</style>
</head>
<body>
<h1>The following files were successfully processed:</h1>
<p>Sample Report (.xml):</p>
<div>$xml_file</div>
<p>Interval Based Report (.xls):</p>
<div>$xls_file</div>
<p>You can find the output file in the following directory: </p>
<div>$outdir</div>
<p>Process more files?</p>
<p><a href="http://localhost/cytoReport.html">Click here to RESTART</a></p>
</body>
</html>
END_HTML

###################################################################