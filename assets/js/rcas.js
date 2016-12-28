var RCAS = (function () {
    var RCAS = {};
    RCAS.toggleLog = function (which) {
        var el = document.getElementById(which);
        el.style.display =
            (el.style.display !== 'block') ? 'block' : 'none';
    };

    // If this request was made by Galaxy: get the GALAXY_URL
    RCAS.galaxyUrl = function () {
        var qstring = window.location.search,
            i, l, temp, params = {}, queries;

        if (qstring.length) {
            queries = qstring.split("?")[1].split("&");
            // Convert the array of strings into an object
            for (i=0, l=queries.length; i<l; i++) {
                temp = queries[i].split('=');
                params[temp[0]] = temp[1];
            }
            return decodeURIComponent(params['GALAXY_URL']) +
                "?tool_id=" + params['tool_id'];
        } else {
            return null;
        }
    };

    // Generate link target to send results back to Galaxy
    RCAS.galaxySend = function (that, result_id) {
        that.href = RCAS.galaxyUrl() +
            '&STATUS=OK&URL=' +
            window.location.origin + '/result/' +
            result_id + '/download';
        return true;
    };

    // If this is used via Galaxy, modify the view.
    RCAS.galaxyInit = function () {
        if (RCAS.galaxyUrl() === null)
            return false;

        var galaxyShow = document.getElementsByClassName('galaxy'),
            galaxyHide = document.getElementsByClassName('galaxy-hide'),
            i;

        for (i=0; i < galaxyShow.length; i++) {
            galaxyShow[i].style.display = 'block';
        }
        for (i=0; i < galaxyHide.length; i++) {
            galaxyHide[i].style.display = 'none';
        }

        return true;
    };

    return RCAS;
})();
