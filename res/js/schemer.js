schemeInterpreter = {};

schemeInterpreter.ajaxEval = function(line, report) {
    console.log("Calling");
    if (schemeInterpreter.uuid === undefined || schemeInterpreter.uuid === null) {
        console.log("Getting UUID");
        $.ajax({
            dataType: "json",
            url: '/getUUID',
            success: function(data, textStatus, jqXHR) {
                console.log(data);
                if (data.uuid) {
                    schemeInterpreter.uuid = data.uuid;
                }
                schemeInterpreter.ajaxEval(line, report);
            },
            async: false
        });
        console.log("Finished getting");
        return;
    }

    var args = {
        'exp': line,
        'uuid': schemeInterpreter.uuid
    };

    (function pollForResult() {
        $.ajax({
            url: '/eval',
            dataType: 'json',
            type: 'GET',
            data: args,
            success: function(result) {
                console.log(result);
                if (result.status === "Invalidated UUID") {
                    schemeInterpreter.uuid = null;
                    schemeInterpreter.ajaxEval(line, report);
                }
                if (result.out === undefined) {
                    setTimeout(pollForResult, 50);
                } else {
                    var msgs = result.out.map(function (s) {
                        return {msg:s,
                                className:"jquery-console-message-value"};
                    });
                    report(msgs);
                }
            }
        });
    }());
}

schemeInterpreter.interpreterInit = function() {
    schemeInterpreter.stdin = [];
    schemeInterpreter.stdout = [];

    schemeInterpreter.interpreter = $('#scheme-interpreter').console({
        promptLabel: '> ',
        commandValidate: function(line) {
            if (line == "") return false;
            return true;
        },
        commandHandle: function(line, report) {
            schemeInterpreter.ajaxEval(line, report);
        },

        animateScroll: true,
        promptHistory: true,
        autofocus: true
    });
};



$(function() {
    schemeInterpreter.interpreterInit();
});
