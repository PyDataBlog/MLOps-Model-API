package DDG::Spice::ShabbatCandleLightingTimes;
# ABSTRACT: Returns Shabbat candle lighting time information from Hebcal
# Start at https://duck.co/duckduckhack/spice_overview if you are new
# to instant answer development

use DDG::Spice;

# Caching - https://duck.co/duckduckhack/spice_advanced_backend#caching-api-responses
spice is_cached => 0;

# Metadata.  See https://duck.co/duckduckhack/metadata for help in filling out this section.
name "Shabbat Candle Lighting Times";
source "Hebcal";
icon_url "https://icons.duckduckgo.com/ip2/www.hebcal.com.ico";
description "Finds candle lighting times for the upcoming Shabbat";
primary_example_queries "candle lighting", "shabbat", "shabbos";
# Uncomment and complete: https://duck.co/duckduckhack/metadata#category
category "location_aware";
# Uncomment and complete: https://duck.co/duckduckhack/metadata#topics
topics "special_interest";
code_url "https://github.com/duckduckgo/zeroclickinfo-spice/blob/master/lib/DDG/Spice/ShabbatCandleLightingTimes.pm";
attribution github => ["bdjnk", "Michael Plotke"],
            twitter => "MichaelPlotke",
            web => "michael.plotke.me";

# API endpoint - https://duck.co/duckduckhack/spice_attributes#spice-codetocode
spice to => 'http://www.hebcal.com/shabbat/?cfg=json&geo=pos&$1';
spice wrap_jsonp_callback => 1;

# Triggers - https://duck.co/duckduckhack/spice_triggers
triggers any => "candle lighting", "shabbat", "shabbos";

my @locs = qw (city region_name country_name );

# Handle statement
handle remainder => sub {

    # optional - regex guard
    # return unless qr/^\w+/;

#    return unless $_;    # Guard against "no answer"
#
#    return $_;

    return if $_;
    
    my $location = join("&", "tzid=", $loc->time_zone, "latitude=", $loc->latitude, "longitude=", $loc->longitude);

    my $loc_str = join " ", map { $loc->{$_} } @locs;
    return $loc_str, 'current', {is_cached => 0};
};

1;
