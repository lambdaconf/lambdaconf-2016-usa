var signet = (function () {
    'use strict';

    var supportedTypes = {
        '()': isType('string'),
        any: function () { return true; },
        '*': function () { return true; },
        array: isInstanceOf(Array),
        boolean: isType('boolean'),
        function: isType('function'),
        number: isType('number'),
        object: isType('object'),
        string: isType('string'),
        symbol: isType('symbol'),
        undefined: isType('undefined')
    };

    // State rules for type lexer

    var lexRules = [
        function initRule(key, types) {
            return types.length === 1 ? 'accept' : 'init';
        },

        function failRule(key, types) {
            return isBadTypeList(types) ? 'fail' : key;
        }

    ];

    var lexStates = {
        init: lexRules,
        accept: lexRules,
        fail: [] // Always fails
    }

    // State rules for type verification

    var verifyRules = [
        function accept(key, typeObj, value) {
            return supportedTypes[typeObj.type](value, typeObj, isTypeOf) ? 'accept' : 'fail';
        },

        function skip(key, typeObj) {
            return typeObj.optional && key === 'fail' ? 'skip' : key;
        }
    ];

    var verificationStates = {
        skip: verifyRules,
        accept: verifyRules,
        fail: []
    };

    // State machine execution

    function updateState(states, stateKey, type, value) {
        var result = stateKey;
        var stateSet = states[stateKey];
        
        for(var i = 0; i < stateSet.length; i++) {
            result = stateSet[i](result, type, value);
        }
        
        return result;
    }

    var updateLexState = updateState.bind(null, lexStates);
    var updateVerificationState = updateState.bind(null, verificationStates);

    // Predicate functions

    function isType(type) {
        return function (value) {
            return type === typeof value;
        }
    }

    function isInstanceOf(obj) {
        return function (value) {
            return value instanceof obj;
        }
    }

    function isBadTypeList(types) {
        return types.length === 0 || types.filter(isTypeInvalid).length > 0;
    }

    function isTypeInvalid(typeObj) {
        var unsupportedType = !isType('function')(supportedTypes[typeObj.type]);
        var unsupportedSubtype = isType('string')(typeObj.subType) && typeObj.type !== 'object';

        return unsupportedType || unsupportedSubtype;
    }

    // Throw on error functions

    function throwOnInvalidSignature(tokenTree, userFn) {
        var shortTokenTree = tokenTree <= 1;
        var typesOkay = tokenTree.reduce(updateLexState, 'init') === 'accept';
        var lengthOkay = tokenTree[0].length >= userFn.length;

        var message = !lengthOkay ?
            'All function parameters are not accounted for in type definition' :
            'Invalid function signature; ensure all input and output paths are valid';

        if (!lengthOkay || shortTokenTree || !typesOkay) {
            throw new Error(message);
        }
    }

    function throwOnTypeState(failState, state, message) {
        if (state === failState) {
            throw new TypeError(message);
        }
    }

    // Utility functions

    function splitSignature(signature) {
        return signature.split('=>');
    }

    // Metadata attachment

    function attachProp(userFn, propName, value) {
        Object.defineProperty(userFn, propName, {
            value: value,
            writeable: false
        });
    }

    function signatureBuilder(tokenTree) {
        return function () {
            return buildSignatureFromTree(tokenTree);
        }
    }

    function attachSignatureData(userFn, tokenTree) {
        if (tokenTree.length > 1) {
            Object.defineProperty(userFn, 'signature', {
                get: signatureBuilder(tokenTree),
                writeable: false
            });

            attachProp(userFn, 'signatureTree', tokenTree);
        }

        return userFn;
    }

    // Type construction
    function isOptionalBracket(token, index) {
        var isOptionalOpen = index === 0 && token[index] === '[';
        var isOptionalClose = index === token.length - 1 && token[index] === ']';

        return isOptionalOpen || isOptionalClose;
    }

    function isWhiteSpace(token, index) {
        return token[index].match(/\s/) !== null;
    }

    function splitOnFirst(delim) {
        return function (token) {
            var result = [];
            var tempValue = '';

            for (var i = 0; i < token.length; i++) {
                if (result.length === 0 && token[i] === delim) {
                    result.push(tempValue);
                    tempValue = token.substring(i + 1, token.length);
                    break;
                } else if (!isOptionalBracket(token, i)) {
                    tempValue += token[i];
                }
            }

            result.push(tempValue);

            return result;
        };
    }

    function splitTypeToken(token, delimiter) {
        var splitToken = splitOnFirst(delimiter)(token);

        if (delimiter === '<' && splitToken[1]) {
            splitToken[1] = splitToken[1].substring(0, splitToken[1].length - 1);
        }

        return splitToken;
    }

    function splitSubTypes(rawToken) {
        var subTypes = [];
        var angleBracketStack = [];
        var tempValue = '';

        for (var i = 0; i < rawToken.length; i++) {
            if (angleBracketStack.length === 0 && rawToken[i] === ';') {
                subTypes.push(tempValue);
                tempValue = '';
            } else {
                tempValue += rawToken[i];
            }

            if (rawToken[i] === '<') {
                angleBracketStack.push('<');
            } else if (rawToken[i] === '>') {
                angleBracketStack.pop();
            }
        }

        subTypes.push(tempValue);

        return subTypes;
    }

    function buildTypeObj(token) {
        var delimiter = token.indexOf('object:') > -1 ? ':' : '<';
        var splitType = splitTypeToken(token, delimiter);

        var type = splitType[0];
        var secondaryType = splitType[1];
        var isValueType = isType('string')(secondaryType) && delimiter === '<';

        return {
            type: type,
            subType: !isValueType ? secondaryType : undefined,
            valueType: isValueType ? splitSubTypes(secondaryType) : undefined,
            optional: token.match(/^\[.*\]$/) !== null
        };
    }

    function buildTypeTree(rawToken) {
        var tokenTree = [];
        var tempValue = '';

        for (var i = 0; i < rawToken.length; i++) {
            if (rawToken[i] === ',') {
                tokenTree.push(buildTypeObj(tempValue));
                tempValue = '';
            } else if (!isWhiteSpace(rawToken, i)) {
                tempValue += rawToken[i];
            }
        }

        tokenTree.push(buildTypeObj(tempValue));

        return tokenTree;
    }

    function buildTypeStr(typeObj) {
        var typeStr = typeObj.type;

        if (!isType('undefined')(typeObj.valueType)) {
            typeStr += '<' + typeObj.valueType.join(';') + '>';
        }

        return typeStr;
    }

    // Verification mutually recursive behavior

    function getNextArgs(state, args) {
        return state === 'skip' ? args : args.slice(1);
    }

    function isVerificationComplete(nextSignature, nextArgs) {
        return nextSignature.length === 0 || nextArgs.length === 0;
    }

    function nextVerificationStep(inputSignature, args, state) {
        var errorMessage = 'Expected type ' + buildTypeStr(inputSignature[0]) + ' but got ' + typeof args[0];

        var nextSignature = inputSignature.slice(1);
        var nextArgs = getNextArgs(state, args);
        var done = isVerificationComplete(nextSignature, nextArgs);

        throwOnTypeState('fail', state, errorMessage);

        return !done ? verifyOnState(nextSignature, nextArgs, state) : state;
    }

    function verifyOnState(inputSignature, args, inState) {
        var state = isType('undefined')(inState) ? 'accept' : inState;
        var outState = updateVerificationState(state, inputSignature[0], args[0]);

        return nextVerificationStep(inputSignature, args, outState);
    }

    // Type enforcement setup and behavior

    function buildWrapperArgs(signedFn, args) {
        var done = signedFn.length <= args.length;

        return !done ? buildWrapperArgs(signedFn, args.concat(['x' + args.length])) : args.join(',');
    }

    function buildSignatureFromTree(tokenTree) {
        var signature = '';
        var tempType;
        var i, j;

        for (i = 0; i < tokenTree.length; i++) {
            if (i > 0) {
                signature += ' => ';
            }

            for (j = 0; j < tokenTree[i].length; j++) {
                if (j > 0) {
                    signature += ', ';
                }
                tempType = buildTypeStr(tokenTree[i][j]);

                if (tokenTree[i][j].optional) {
                    tempType = '[' + tempType + ']';
                }

                signature += tempType;
            }
        }

        return signature;
    }

    function throwOnTypeMismatch(type, value) {
        if (!isTypeOf(type)(value)) {
            throw new Error('Expected return value of type ' + resultType + ' but got ' + typeof result);
        }
    }

    function callAndEnforce(signedFn, args) {
        verify(signedFn, args);

        var result = signedFn.apply(null, args);
        var tokenTree = signedFn.signatureTree.slice(1);
        var expectedType = tokenTree.length > 1 ? 'function' : buildTypeStr(tokenTree[0][0]);

        throwOnTypeMismatch(expectedType, result);
        attachSignatureData(result, tokenTree);

        return tokenTree.length > 1 ? enforce(result) : result;
    }

    function buildEnforceWrapper(signedFn) {
        var wrapperTemplate = 'return function enforceWrapper (' + buildWrapperArgs(signedFn, []) + ') {' +
            'return callAndEnforce(signedFn, Array.prototype.slice.call(arguments));' +
            '};';

        var wrapperFn = new Function(['signedFn', 'callAndEnforce'], wrapperTemplate);

        return wrapperFn(signedFn, callAndEnforce);
    }

    // Core functionality

    function sign(signature, userFn) {
        var tokenTree = splitSignature(signature).map(buildTypeTree);

        throwOnInvalidSignature(tokenTree, userFn);

        return attachSignatureData(userFn, tokenTree);
    }

    function verify(signedFn, args) {
        var finalState = verifyOnState(signedFn.signatureTree[0], Array.prototype.slice.call(args, 0));

        throwOnTypeState('skip', finalState, 'Optional types were not fulfilled properly');
    }

    function enforce(signedFn) {
        var enforcementWrapper = buildEnforceWrapper(signedFn);

        attachSignatureData(enforcementWrapper, signedFn.signatureTree);
        attachProp(enforcementWrapper, 'toString', signedFn.toString.bind(signedFn));

        return enforcementWrapper;
    }

    var signAndEnforce = function (signature, userFn) {
        return enforce(sign(signature, userFn));
    }

    function extend(key, predicate) {
        if (typeof supportedTypes[key] !== 'undefined') {
            throw new Error('Cannot redefine type ' + key);
        }

        supportedTypes[key] = predicate;
    }

    function subtype(existingType) {
        var typeSignature = existingType + ', object, function => boolean';

        return function (key, predicate) {
            var enforcedPredicate = signAndEnforce(typeSignature, predicate);
            extend(key, enforcedPredicate);
        }
    }

    function alias(key, typedef) {
        extend(key, isTypeOf(typedef));
    }

    function isTypeOf(typeStr) {
        var typeObj = buildTypeObj(typeStr);

        return function (value) {
            var result = true;

            try {
                result = supportedTypes[typeObj.type](value, typeObj);
            } catch (e) {
                result = false;
            }

            return result;
        };
    }

    // Final module definition

    var signet = {
        alias: signAndEnforce('string, string => undefined', alias),
        enforce: signAndEnforce('string, function => function', signAndEnforce),
        extend: signAndEnforce('string, function => undefined', extend),
        isTypeOf: signAndEnforce('string => * => boolean', isTypeOf),
        sign: signAndEnforce('string, function => function', sign),
        subtype: signAndEnforce('string => string, function => undefined', subtype),
        verify: signAndEnforce('function, object => undefined', verify)
    };

    if (typeof module !== 'undefined' && typeof module.exports !== 'undefined') {
        module.exports = signet;
    }

    return signet;

})();