var app = angular.module('zotonic', []);
app.value('uniqueID', function(n) {
    return Math.random().toString(36).substr(2, n);
});

app.config(function($routeProvider, $interpolateProvider) {
    $interpolateProvider.startSymbol('[[').endSymbol(']]');
});

app.factory(
    'zotonicSocket', 
    function($q, $rootScope, uniqueID) {

        return function(socketURL) {
            socketURL = socketURL || '/ng/socket';
            
            var calls = {};
            var connected = false;
            var connectQueue = [];
            var q_onopen = $q.defer();
            var q_onclose = $q.defer();

            var loader = function(flag) {};
            
            var onMessage = function(m) {
                console.warn('ng-zotonic.js: Got message, but no onMessage handler attached!', m);
            };

            if (socketURL.match(/^\//)) {
                socketURL = "ws://" + document.location.host + socketURL;
            }
            
            var ws = new WebSocket(socketURL);

            ws.onopen = function() {
                $rootScope.$apply(function() {
                    connected = true;
                    console.log('Connected to ' + socketURL);
                    angular.forEach(connectQueue, function(msg) {
                        send(msg);
                    });
                    connectQueue = [];
                    q_onopen.resolve();
                });
            };

            ws.onclose = function() {
                $rootScope.$apply(function() {
                    connected = false;
                    console.log('Disconnected from ' + socketURL);
                    q_onclose.resolve();
                });
            };

            ws.onmessage = function(m) {
                $rootScope.$apply(function() {
                    var msg = JSON.parse(m.data);

                    if (typeof msg.reply_id != 'undefined') {
                        // response to a call
                        // FIXME how to handle an unknown call_id?
                        if (!msg.error) {
                            calls[msg.reply_id].resolve(msg.reply);
                        } else {
                            console.error(msg.error);
                            calls[msg.reply_id].reject(msg.error);
                        }                            
                        delete calls[msg.reply_id];
                        loader(false);
                        return;
                    }

                    onMessage(msg);
                });
            };

            function send(payload) {
                if (connected) {
                    ws.send(payload);
                } else {
                    connectQueue.push(payload);
                }
            }
            
            return {
                onopen: q_onopen.promise,
                setOnMessage: function(m) {
                    onMessage = m;
                },

                // Loader function gets called with loader(true) when
                // a call() starts, and loader(false) when it returns.
                setLoader: function(l) {
                    loader = l;
                },
                
                // Call function; can have a reply
                call: function(module, command, args) {
                    args = args || {};
                    var id = uniqueID(8);
                    calls[id] = $q.defer();
                    $rootScope.$apply(function() {
                        loader(true);
                    });
                    send("call:" + id + ":" + module + ":" + command + ":" + JSON.stringify(args));
                    return calls[id].promise;
                },

                // Simple cast function, without reply
                cast: function(module, command, args) {
                    args = args || {};
                    send("cast:" + module + ":" + command + ":" + JSON.stringify(args));
                }
            };
        };
    });


/**
 * Either: <z-template include="foo.tpl" /> or
 * <z-template catinclude="foo.tpl" id="23" /> 
 */
app.directive("zTemplate", function($compile, appSocket) {
    return {
        restrict: 'E',
        link: function(scope, elem, attrs) {
            var args = {};
            angular.forEach(attrs.$attr, function(k) { args[k] = attrs[k]; });
            appSocket().call("ng_template", "render", args).then(
                function(html) {
                    elem.append(
                        $compile(html)(scope)
                    );
                }
            );
        }
    };
});
