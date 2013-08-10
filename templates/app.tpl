<!DOCTYPE html>
<html ng-app="example">
    <head>
        <title>Angular JS module example</title>

        {% lib
            "js/vendor/jquery.min.js"
            "js/vendor/moment.min.js"
            "js/vendor/angular.min.js"
            "js/ng-zotonic.js"
            "js/ng-example.js"
        %}
    </head>

    <body>
        <app-init />
        <jqm-view></jqm-view>
    </body>

</html>
