var ewg_mapinfo = [];

function to_rad(dec) {
    return dec * Math.PI/180;
};

function haversine_dist(lat1, lon1, lat2, lon2) {
    // all credits go to http://www.movable-type.co.uk/scripts/latlong.html
    var R = 6371000; // m
    var dLat = to_rad(lat2-lat1);
    var dLon = to_rad(lon2-lon1);
    var latr1 = to_rad(lat1);
    var latr2 = to_rad(lat2);
    var a = Math.sin(dLat/2) * Math.sin(dLat/2) +
            Math.sin(dLon/2) * Math.sin(dLon/2) * Math.cos(latr1) * Math.cos(latr2);
    var c = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1-a));
    var d = (R * c) / Math.sqrt(2); // there is an odd 1.4 multiplier in this copied alg.
    return d;
};

function pointatdist(lat, lon, bearing, distance) {
    var d = distance/6371000;
    var lat1 = to_rad(lat);
    var lon1 = to_rad(lon);
    var brng = to_rad(bearing);
    var lat2 = Math.asin( Math.sin(lat1)*Math.cos(d) +
              Math.cos(lat1)*Math.sin(d)*Math.cos(brng) );
    var lon2 = lon1 + Math.atan2(Math.sin(brng)*Math.sin(d)*Math.cos(lat1),
                     Math.cos(d)-Math.sin(lat1)*Math.sin(lat2));
    return [(lat2 * 180/Math.PI), (lon2 * 180/Math.PI)];
};

function ewg_draw_shapes(targetdiv, shapes) {
    var tile_url;
    var map = L.map(targetdiv, {attributionControl: true});
    var layergroup = L.layerGroup().addTo(map);
    var latmax = -400;
    var latmin = 400;
    var lonmax = -400;
    var lonmin = 400;
    var latmid;
    var lonmid;
    var dist;
    var zoom;
    $.each(shapes, function(i1, shape) {
        if (shape.type == "polygon") {
            $.each(shape.points , function(i2, point) {
                if (point[0] > latmax) {latmax = point[0]};
                if (point[0] < latmin) {latmin = point[0]};
                if (point[1] > lonmax) {lonmax = point[1]};
                if (point[1] < lonmin) {lonmin = point[1]};
            })
        } else if (shape.type == "marker") {
            if (shape.position[0] > latmax) {latmax = shape.position[0]};
            if (shape.position[0] < latmin) {latmin = shape.position[0]};
            if (shape.position[1] > lonmax) {lonmax = shape.position[1]};
            if (shape.position[1] < lonmin) {lonmin = shape.position[1]};
        } else if (shape.type == "polyline_arrow") {
            $.each(shape.points , function(i2, point) {
                if (point[0] > latmax) {latmax = point[0]};
                if (point[0] < latmin) {latmin = point[0]};
                if (point[1] > lonmax) {lonmax = point[1]};
                if (point[1] < lonmin) {lonmin = point[1]};
            })
        }
    });
    latmid = (latmax+latmin)/2;
    lonmid = (lonmax+lonmin)/2;
    // approximate autozoom
    dist = haversine_dist(latmin, lonmin, latmax, lonmax);
    if (dist <500) {
        zoom = 16;
    } else if (dist < 1000) {
        zoom = 15;
    } else if (dist <5000 ) {
        zoom = 13;
    } else if (dist <10000 ) {
        zoom = 12;
    } else if (dist < 20000) {
        zoom = 11;
    } else if (dist < 40000) {
        zoom = 10;
    } else if (dist < 80000) {
        zoom = 9;
    } else {
        zoom = 7;
    };
    map.setView([latmid, lonmid], zoom);
    if (map_rproxy == true) {
        tile_url = '/openstreetmap/{z}/{x}/{y}.png';
    } else {
        tile_url = 'http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png';
    }; 
    L.tileLayer(tile_url, {
        maxZoom: 17,
        attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a>'
    }).addTo(map);
    layergroup.clearLayers();
    $.each(shapes, function(i, shape) {
        var p;
        if (shape.type == "polygon") {
            p = L.polygon(shape.points, {color : shape.color, fillOpacity: 0.2, opacity: 0.4});
            layergroup.addLayer(p);
        } else if (shape.type == "marker") {
            p = L.marker(shape.position);
            layergroup.addLayer(p);
        } else if (shape.type = "polyline_arrow") {
            var pline = L.polyline(shape.points, {color: shape.color});
            p = pline;
            layergroup.addLayer(pline);
            layergroup.addLayer(L.polylineDecorator(pline, {
                patterns: [{
                    offset: 10, repeat: 200,
                    symbol: L.Symbol.arrowHead({pixelSize: 15, pathOptions: {fillOpacity: 0.7, weight: 0}})
                }]
            }));
        };
        if (shape.text != "") {
            p.bindLabel(shape.text);
        };
    });
};

function ewg_set_shapes(targetid, data) {
    ewg_mapinfo.push({"target": targetid, "shapes" : data});
};

$(function() {
    $.each(ewg_mapinfo, function(i, d) {
        ewg_draw_shapes("map", d.shapes);
    });
});
