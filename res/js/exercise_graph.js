function d3plot(idata) {
    var data = [];
    var t;

    for (var elem in idata) {
        t = idata[elem];
        t.date = elem;
        data.push(t);
    }

    function comp(a, b) {
        if (a.date < b.date)
            return -1;
        if (a.date > b.date)
            return 1;
        return 0;
    }
    data.sort(comp);

    $( ".graph-region" ).empty();
    var is_weight_graph = ("body_weight" in idata[Object.keys(idata)[0]]);
    var is_bodyweight_ex_graph = !is_weight_graph && (idata[Object.keys(idata)[0]]["weight"] == null);

    var margin = {top: 20, right: 20, bottom: 30, left: 50},
        width = 600 - margin.left - margin.right,
        height = 400 - margin.top - margin.bottom;

    var parseDate = d3.time.format("%m-%d-%Y").parse;

    var x;
    if (is_weight_graph || is_bodyweight_ex_graph) { // use a date axis
        x = d3.time.scale()
            .range([0, width]);
    } else {
        x = d3.scale.linear()
            .range([0, width]);
    }

    var y = d3.scale.linear()
        .range([height, 0]);

    var xAxis = d3.svg.axis()
        .scale(x)
        .orient("bottom");

    var yAxis = d3.svg.axis()
        .scale(y)
        .orient("left");

    var svg = d3.select(".graph-region").append("svg")
        .attr("width", width + margin.left + margin.right)
        .attr("height", height + margin.top + margin.bottom)
        .append("g")
        .attr("transform", "translate(" + margin.left + "," + margin.top + ")");

    var min_x = Infinity, max_x = -Infinity;
    var min_y = Infinity, max_y = -Infinity;

    $.each(data, function(i) {
        value = data[i];
        if (is_weight_graph || is_bodyweight_ex_graph) {
            temp = parseDate(value.date);
        } else {
            temp = value.weight;
        }
        if( temp < min_x) min_x = temp;
        if( temp > max_x) max_x = temp;

        if (is_weight_graph) {
            if( value.body_weight < min_y) min_y = value.body_weight;
            if( value.body_weight > max_y) max_y = value.body_weight;
        } else {
            temp = value.repetitions * value.sets;
            if( temp < min_y ) min_y = temp;
            if( temp > max_y ) max_y = temp;
        }
    });

    var adjust = 0.05*(max_y - min_y);
    x.domain([min_x, max_x]);
    y.domain([min_y - adjust, max_y + adjust]);

    var line = d3.svg.line()
        .x(function(d) {
            if (is_weight_graph || is_bodyweight_ex_graph) {
                return x(parseDate(d.date));
            }
            return x(d.weight);
        })
        .y(function(d) {
            if (is_weight_graph) {
                return y(d.body_weight);
            }
            return y(d.repetitions * d.sets);
        })
        .interpolate("basis");

    svg.selectAll("path").data([data]).enter().append("path")
        .attr("class", "line")
        .attr("d", line);

    var y_label = "Total repetitions";
    var x_label = "Weight (lbs)";

    if (is_weight_graph || is_bodyweight_ex_graph) {
        x_label = "Date";
    }

    if (is_weight_graph) {
        y_label = "Bodyweight (lbs)";
    }

    svg.append("g")
        .attr("class", "x axis")
        .attr("transform", "translate(0," + height + ")")
        .call(xAxis);


    svg.append("g")
        .attr("class", "y axis")
        .call(yAxis)
        .append("text")
        .attr("transform", "rotate(-90)")
        .attr("y", 6)
        .attr("dy", ".71em")
        .style("text-anchor", "end");

    svg.append("text")
        .attr("text-anchor", "middle")  // this makes it easy to centre the text as the transform is applied to the anchor
        .attr("transform", "translate("+ (margin.left)/2 +","+(height/2)+")rotate(-90)")  // text is drawn off the screen top left, move down and out and rotate
        .text(y_label);

    svg.append("text")
        .attr("text-anchor", "middle")  // this makes it easy to centre the text as the transform is applied to the anchor
        .attr("transform", "translate("+ (width/2) +","+(height-(margin.bottom)/3)+")")  // centre below axis
        .text(x_label);
}

$(document).ready(function(){
    $("#submit").click(function(){
        var n = $( "input[name='name']" ).val();
        n = n.replace(/ /g, "_");
        if (n) {
            var submissionURL = '/exercise/data/' + n
            $.getJSON(submissionURL, function(data, textStatus, jqXHR){
                d3plot(data);
            });
        }
        $( "input[name='name']" ).val("");
    });
});
