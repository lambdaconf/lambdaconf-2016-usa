// module Game.Driver

exports._runGame = function(either, game) {
  return function() {
    var state = game.initial;

    var button  = document.querySelector("#button");
    var input   = document.querySelector("#input");
    var content = document.querySelector("#content");

    content.innerHTML = game.describe(state);

    var onInput = function(e) {
      var cmd = input.value;
      var parsed = game.parse(cmd.toLowerCase());

      either(function(error) {
        content.innerHTML += "<br><br>" + error;
      })(function(input) {
        either(function(error) {
          content.innerHTML += "<br><br>" + error;
        })(function(state2) {
          state = state2;

          content.innerHTML += "<br><br>" + cmd + "<br><br>" + game.describe(state);
        })(game.update(state)(input));
      })(parsed);

      input.value = '';
    };

    button.onclick = onInput;

    input.onkeypress = function(e) {
      if (e.keyCode == 13) {
        e.preventDefault();
        onInput(e);
      }
    };
  };
}
