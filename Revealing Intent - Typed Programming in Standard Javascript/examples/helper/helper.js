var displayRunner = (function () {
    'use strict';
    
    function runAndReturn (fxn){
        var output = '';
        
        try {
            output = fxn();
        } catch (e) {
            console.log(e);
            output = 'Error: ' + e.message;
        }
        
        return output;
    }
    
    function writeTo (id){
        var logElement = document.getElementById(id);
        logElement.innerHTML = '';
        
        return function (output) {
            var preparedOutput = typeof output === 'string' ? output : JSON.stringify(output);
            var formattedOutput = preparedOutput.replace(/\</g, '&lt;').replace(/\>/g, '&gt;');
            logElement.innerHTML += formattedOutput + '<br />';
        }
    }
    
    function composeTimedWriter (id){
        var writeln = writeTo(id);
        return function (next, current) {
            return function () {
                writeln(runAndReturn(current));
                setTimeout(next, 200);
            }
        }
    }
    
    function displayRunner (id, fxnList){
        document.getElementById(id + '-wrapper').style.display = 'block';
        fxnList.reverse().reduce(composeTimedWriter(id), function () {})();
    }
    
    return displayRunner;
})();