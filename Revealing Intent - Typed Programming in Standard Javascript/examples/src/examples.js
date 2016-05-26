function throwOnArrayMismatch(value) {
    if (!(value instanceof Array)) {
        throw new Error('Expected array but got ' + typeof value);
    }
}

function checkType(type, value) {
    if (!signet.isTypeOf(type)(value)) {
        var errorMessage =
            'Expected value of type ' +
            type + ' but got ' +
            typeof value +
            ': ' + JSON.stringify(value);

        throw new Error(errorMessage);
    }

    return JSON.stringify(value) + ' is of type ' + type;
}

function partial(fxn) {
    var args = Array.prototype.slice.call(arguments, 1);

    return function () {
        return fxn.apply(null, args);
    }
}

var bareVectors = (function () {
    'use strict';

    function vectorAdd(v1, v2) {
        return [v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2]];
    }

    return [
        vectorAdd.bind(null, [1, 2, 3], [4, 5, 6]),
        vectorAdd.bind(null, [1, 2, 3, 4], [5, 6, 7]),
        vectorAdd.bind(null, 'foo', 'bar')
    ];
})();

var simpleCheck = (function () {
    'use strict';

    function vectorAdd(v1, v2) {
        throwOnArrayMismatch(v1);
        throwOnArrayMismatch(v2);

        return [v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2]];
    }

    return [
        vectorAdd.bind(null, [1, 2, 3], [4, 5, 6]),
        vectorAdd.bind(null, [1, 2, 3, 4], [5, 6, 7]),
        vectorAdd.bind(null, 'foo', 'bar')
    ];
})();

var signedFn = (function () {
    'use strict';

    vectorAdd.signature = 'array, array => array';

    function vectorAdd(v1, v2) {
        throwOnArrayMismatch(v1);
        throwOnArrayMismatch(v2);

        return [v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2]];
    }

    return [
        (function (fn) { return fn.signature }).bind(null, vectorAdd),
        
        vectorAdd.bind(null, [1, 2, 3], [4, 5, 6]),
        vectorAdd.bind(null, [1, 2, 3, 4], [5, 6, 7]),
        vectorAdd.bind(null, 'foo', 'bar')
    ];
})();

var signetEnforced = (function () {
    'use strict';

    var vectorAdd = signet.enforce(
        'array, array => array',
        function vectorAdd(v1, v2) {
            return [v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2]];
        });

    return [
        (function (fn) { return fn.signature }).bind(null, vectorAdd),
        
        vectorAdd.bind(null, [1, 2, 3], [4, 5, 6]),
        vectorAdd.bind(null, [1, 2, 3, 4], [5, 6, 7]),
        vectorAdd.bind(null, 'foo', 'bar')
    ];
})();

var lowerCaseConstruction = (function () {
    'use strict';
    
    function lowerCaseType (value){
        return typeof value === 'string' && value.toLowerCase() === value;
    }
    
    signet.extend('lowerCase', lowerCaseType);
    
    return [
        partial(checkType, 'lowerCase', 'foo'),
        partial(checkType, 'lowerCase', 'bar'),
        partial(checkType, 'lowerCase', 'BaZ'),
    ];
})();

var ageConstruction = (function () {
    'use strict';

    signet.subtype('number')('int', function (value) {
        return value === Math.floor(value);
    });

    signet.subtype('int')('natural', function (value) {
        return value >= 0;
    });

    signet.subtype('natural')('age', function (value) {
        return value <= 150;
    });

    return [
        partial(checkType, 'int', 3.2),
        partial(checkType, 'int', -3),
        partial(checkType, 'natural', -3),
        partial(checkType, 'natural', 200),
        partial(checkType, 'age', 200),
        partial(checkType, 'age', 42)
    ];
})();

var tupleConstruction = (function () {
    'use strict';

    function verifyType(tuple, type, index) {
        return signet.isTypeOf(type)(tuple[index]);
    }

    function verifyTuple(tuple, typeObj) {
        return typeObj.valueType
            .map(verifyType.bind(null, tuple))
            .reduce(function (a, b) { return a && b; }, true);
    }

    signet.subtype('array')('tuple', tupleType);

    function tupleType(tuple, typeObj) {
        var correctLength = tuple.length === typeObj.valueType.length;
        var correctTypes = verifyTuple(tuple, typeObj);

        return correctLength && correctTypes;
    }

    signet.subtype('string')('name', function (value) {
        return value.length < 20;
    });

    return [
        partial(checkType, 'tuple<number;number>', [3, 4]),
        partial(checkType, 'tuple<age;name>', [21, 'Bobby']),
        partial(checkType, 'tuple<age;name>', [5, 99])
    ];
})();

var formattedStringConstruction = (function () {
    'use strict';

    signet.subtype('string')('formattedString', function (value, typeObj) {
        var pattern = new RegExp(typeObj.valueType[0]);
        console.log(typeObj.valueType, pattern, value.match(pattern));
        return value.match(pattern) !== null;
    });

    var ssnPattern = '^[0-9]{3}\\-[0-9]{2}\\-[0-9]{4}$';
    var ssnDefinition = 'formattedString<' + ssnPattern + '>';

    signet.alias('ssn', ssnDefinition);
    
    return [
        partial(checkType, 'ssn', '123-45-6789'),
        partial(checkType, 'ssn', '123456789'),
        partial(checkType, 'ssn', 'FOO')
    ];
})();

var vectorAdditionFinal = (function () {
    'use strict';

    signet.alias('3dVector', 'tuple<number;number;number>');
    
    var vectorAdd = signet.enforce(
    '3dVector, 3dVector => 3dVector',
    function vectorAdd(v1, v2) {
        return [v1[0] + v2[0], v1[1] + v2[1], v1[2] + v2[2]];
    });

    return [
        vectorAdd.bind(null, [1, 2, 3], [4, 5, 6]),
        vectorAdd.bind(null, [1, 2, 3, 4], [5, 6, 7]),
        vectorAdd.bind(null, 'foo', 'bar')
    ];

    
})();