var RCAS = (function () {
    var RCAS = {};
    RCAS.toggleLog = function (which) {
        var el = document.getElementById(which);
        el.style.display =
            (el.style.display !== 'block') ? 'block' : 'none';
    };
    return RCAS;
})();
