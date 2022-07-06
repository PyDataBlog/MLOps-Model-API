
    
    
    $.ajax({
            async: false,
            url: "http://api.visalus.com/ITReporting/SalesAnalytics/GetDataBySP/?SPName=usp_PROMO_ViCrunch3FF_JSON&?",
            type: 'GET',
            dataType: 'jsonp',
            success: function (data) {
                var result = JSON.stringify(data);
                //alert(result);
                var obj = jQuery.parseJSON(result);
                var output="<table class='standings-table' width='100%'><tr class='head'><td align='center'>Rank</td><td>Name</td><td>Location</td><td align='center'>VIP Tier</td></tr>";
        for (var i in obj) {
            output+="<tr><td align='center'>" + obj[i].Rank +  "</td><td>" + obj[i].Name + "</td><td>"  + obj[i].Location + "</td><td align='center'>"  + obj[i].VIPtier + "</td></tr>";
        }

        output+="</table>";
                
                document.getElementById("crunchtime").innerHTML = output;
            },
            error: function (e) {
                alert('fail');
            }
        });
 
   
    