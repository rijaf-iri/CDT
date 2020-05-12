
StnChkCoordsHtml <- function(){
tmp <- '<!DOCTYPE html>
<html>

<head>
    <title> Stations coordinates </title>
    <link rel="stylesheet" href="http://cdn.leafletjs.com/leaflet-0.7.3/leaflet.css" />
    <script src="https://ajax.googleapis.com/ajax/libs/jquery/3.5.1/jquery.min.js"></script>
    <script src="http://cdn.leafletjs.com/leaflet-0.7.3/leaflet.js"></script>
    <!-- <script src="https://maps.googleapis.com/maps/api/js?key=YOUR_API_KEY" async defer></script> -->
    <style>
    html,
    body {
        height: 100%;
    }

    #mapAWSCoords {
        width: 100%;
        height: 95%;
        border: 1px solid #AAA;
    }
    </style>
</head>

<body>
    <div>
        <span>
            <label>Change map style:</label>
            <select name="maptype" id="maptype" class="maptype">
                <option value="openstreetmap" selected>OpenStreetMap Standard</option>
                <option value="mapboxsatellitestreets">Mapbox Satellite Streets</option>
                <option value="mapboxsatellite">Mapbox Satellite</option>
                <option value="mapboxstreets">Mapbox Streets</option>
                <option value="mapboxoutdoors">Mapbox Outdoors</option>
                <option value="mapboxlight">Mapbox Light</option>
                <option value="googlemaps">Google Maps</option>
            </select>
        </span>
    </div>
    <div id="mapAWSCoords"></div>
    <script>
    var serverPath = "http://127.0.0.1:25782/custom/CDT/";
    $(document).ready(function() {
        var mymap = L.map("mapAWSCoords", {
            center: [-2.01922938104402, 29.8377173354088],
            minZoom: 2,
            zoom: 8
        });
        var mytile = "";
        mytile = L.tileLayer("http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
            attribution: \'&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>\',
            maxZoom: 19,
            subdomains: ["a", "b", "c"]
        }).addTo(mymap);
        var iconshadow = serverPath + "marker-shadow.png";
        var blueIcon = new L.Icon({
            iconUrl: serverPath + "marker-icon-blue.png",
            shadowUrl: iconshadow,
            iconSize: [25, 41],
            iconAnchor: [12, 41],
            popupAnchor: [1, -34],
            shadowSize: [41, 41]
        });
        var orangeIcon = new L.Icon({
            iconUrl: serverPath + "marker-icon-orange.png",
            shadowUrl: iconshadow,
            iconSize: [25, 41],
            iconAnchor: [12, 41],
            popupAnchor: [1, -34],
            shadowSize: [41, 41]
        });
        var redIcon = new L.Icon({
            iconUrl: serverPath + "marker-icon-red.png",
            shadowUrl: iconshadow,
            iconSize: [25, 41],
            iconAnchor: [12, 41],
            popupAnchor: [1, -34],
            shadowSize: [41, 41]
        });
        var greenIcon = new L.Icon({
            iconUrl: serverPath + "marker-icon-green.png",
            shadowUrl: iconshadow,
            iconSize: [25, 41],
            iconAnchor: [12, 41],
            popupAnchor: [1, -34],
            shadowSize: [41, 41]
        });
        var icons = {
            blue: {
                icon: blueIcon
            },
            orange: {
                icon: orangeIcon
            },
            red: {
                icon: redIcon
            }
        };
        $.getJSON(serverPath + "station_coords.json", function(json) {
            $.each(json, function() {
                var contenu = "";
                if (typeof this.LonX !== "undefined") {
                    L.marker([this.LatX, this.LonX], { icon: icons[this.StatusX].icon }).bindPopup(contenu).addTo(mymap);
                }
            });
        });
        $("#maptype").on("change", function() {
            mymap.removeLayer(mytile);
            mymap.attributionControl.removeAttribution();
            var maptype = $("#maptype option:selected").val();
            if (maptype == "openstreetmap") {
                mytile = L.tileLayer("http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png", {
                    attribution: \'&copy; <a href="https://www.openstreetmap.org/copyright">OpenStreetMap</a>\',
                    maxZoom: 19,
                    subdomains: ["a", "b", "c"]
                }).addTo(mymap);
            } else if (maptype == "googlemaps") {
                mytile = L.tileLayer("http://{s}.google.com/vt/lyrs=s&x={x}&y={y}&z={z}", {
                    attribution: "&copy; Google Maps",
                    maxZoom: 20,
                    subdomains: ["mt0", "mt1", "mt2", "mt3"]
                }).addTo(mymap);
            } else {
                var mapid = "";
                if (maptype == "mapboxsatellite") {
                    mapid = "satellite-v9";
                } else if (maptype == "mapboxstreets") {
                    mapid = "streets-v11";
                } else if (maptype == "mapboxoutdoors") {
                    mapid = "outdoors-v11";
                } else if (maptype == "mapboxlight") {
                    mapid = "light-v10";
                } else {
                    mapid = "satellite-streets-v11";
                }
                mytile = L.tileLayer("https://api.mapbox.com/styles/v1/mapbox/{id}/tiles/{z}/{x}/{y}?access_token={accessToken}", {
                    attribution: \'Map data &copy; <a href="https://www.openstreetmap.org/">OpenStreetMap</a> contributors, <a href="https://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>, Imagery &copy; <a href="https://www.mapbox.com/">Mapbox</a>\',
                    maxZoom: 23,
                    id: mapid,
                    accessToken: "pk.eyJ1IjoibWFwYm94IiwiYSI6ImNpejY4M29iazA2Z2gycXA4N2pmbDZmangifQ.-g_vE53SD2WrJ6tFX7QHmA"
                }).addTo(mymap);
            }
            window.mytile = mytile;
        });
        var marker1;
        mymap.on("dblclick", function(e) {
            if (!marker1) {
                marker1 = L.marker(e.latlng, { icon: greenIcon }).addTo(mymap);
            } else {
                marker1.setLatLng(e.latlng);
            }
            var position = marker1.getLatLng();
            marker1.bindPopup("<b>Latitude : </b>" + position.lat + "<br />" + "<b>Longitude : </b>" + position.lng).openPopup();
        });
    });
    </script>
</body>

</html>
'
	file <- file.path(tempdir(), 'tmp.StnChkCoords.html')
	cat(tmp, file = file)
	tmp <- readLines(file)
	unlink(file)

	return(tmp)
}

