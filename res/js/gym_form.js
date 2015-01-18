$(document).ready(function(){
    $("#submit").click(function(){
        var n = $( "input[name='name']" ).val();
        var w = $( "input[name='weight']" ).val();
        var r = $( "input[name='repetitions']" ).val();
        var s = $( "input[name='sets']" ).val();
        n = n.replace(/ /g, "_");
        if (n) {
            var submissionURL = '/exercise/' + n + '/weight' + w + '/repetitions' + r
                + '/sets' + s
            $.getJSON(submissionURL, function(data, textStatus, jqXHR){});
        }
        $( "input[name='name']" ).val("");
        $( "input[name='weight']" ).val("");
        $( "input[name='repetitions']" ).val("");
        $( "input[name='sets']" ).val("");
    });
});
