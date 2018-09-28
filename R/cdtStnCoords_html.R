
StnChkCoordsHtml <- function(){
tmp <- '<!DOCTYPE html>
<html>

<head>
    <title> Stations coordinates </title>
    <style>
    #map {
        height: 100%;
        width: 100%;
    }
    
    html,
    body {
        height: 100%;
        margin: 0;
        padding: 0;
    }
    </style>
</head>

<body>
    <div id="map"></div>
    <script>
    var map;
    var infoWindow;

    function initMap() {
        var latlngcentre = new google.maps.LatLng(-18.5, 47);
        var mapOptions = {
            zoom: 12,
            center: latlngcentre,
            mapTypeId: google.maps.MapTypeId.HYBRID,
            navigationControl: true,
            scrollwheel: true
        };
        map = new google.maps.Map(document.getElementById("map"), mapOptions);
        map.fitBounds(new google.maps.LatLngBounds(
            new google.maps.LatLng(-26, 42),
            new google.maps.LatLng(-11, 52)));
        infoWindow = new google.maps.InfoWindow();
        var script = document.createElement("script");
        script.src = "http://127.0.0.1:25782/custom/CDT/station_coords.js";
        document.getElementsByTagName("head")[0].appendChild(script);
        var icon1 = {
            url: "https://icons.iconarchive.com/icons/icons-land/vista-map-markers/48/Map-Marker-Push-Pin-1-Chartreuse-icon.png",
            scaledSize: new google.maps.Size(30, 30),
            origin: new google.maps.Point(0, 0),
            anchor: new google.maps.Point(15, 30)
        };
        var marker1 = new google.maps.Marker({
            map: map,
            icon: icon1
        });
        map.addListener("rightclick", function(e) {
            marker1.setPosition(e.latLng);
            infoWindow.setContent("Latitude : " + e.latLng.lat() +
                "<br>" + "Longitude : " + e.latLng.lng());
            infoWindow.open(map, marker1);
        });
    }
    window.stncrds_callback = function(stn) {
        var pinRED = new google.maps.MarkerImage(
            "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|FF0000",
            null,
            null,
            null,
            new google.maps.Size(21, 34)
        );
        var pinORNG = new google.maps.MarkerImage(
            "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|FF9900",
            null,
            null,
            null,
            new google.maps.Size(21, 34)
        );
        var pinBLUE = new google.maps.MarkerImage(
            "http://chart.apis.google.com/chart?chst=d_map_pin_letter&chld=%E2%80%A2|0080FF",
            null,
            null,
            null,
            new google.maps.Size(21, 34)
        );
        var icons = {
            blue: {
                icon: pinBLUE
            },
            orange: {
                icon: pinORNG
            },
            red: {
                icon: pinRED
            }
        };
        for (var i = 0; i < stn.stncoord.length; i++) {
            var data = stn.stncoord[i];
            var latLng = new google.maps.LatLng(data.LatX, data.LonX);
            var titre = "ID : " + data.ID + "\\r" + "Name : " + data.Name + "\\r" + "District : " + data.district + "\\r" + "info : " + data.info;
            var marker = new google.maps.Marker({
                map: map,
                position: latLng,
                title: titre,
                icon: icons[data.StatusX].icon
            });
            (function(marker, data) {
                google.maps.event.addListener(marker, "click", function(e) {
                    var contenu = "ID : " + data.ID + "<br>" + "Name : " + data.Name + "<br>" + "District : " + data.district + "<br>" + "info : " + data.info;
                    infoWindow.setContent(contenu);
                    infoWindow.open(map, marker);
                });
            })(marker, data);
        }
    };
    </script>
    <script src="https://maps.googleapis.com/maps/api/js?callback=initMap"></script>
</body>

</html>
'
	file <- file.path(tempdir(), 'tmp.StnChkCoords.html')
	cat(tmp, file = file)
	tmp <- readLines(file)
	unlink(file)

	return(tmp)
}

