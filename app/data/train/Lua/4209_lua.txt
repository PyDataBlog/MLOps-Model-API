local addon = select(2, ...)

local testsByAddon = {}

local function equals(left, right)
	if type(left) == 'number' and type(right) == 'number' then
		return math.abs(left - right) < 0.0001
	end
	return left == right
end

local function deepEquals(left, right)
	if equals(left, right) then
		return true
	end
	if type(left) ~= 'table' or type(right) ~= 'table' then
		return false
	end
	for key, leftVal in pairs(left) do
		if not deepEquals(leftVal, right[key]) then
			return false
		end
	end
	for key in pairs(right) do
		if left[key] == nil then
			return false
		end
	end
	return true
end

local function formattedAssert(value, assertType, actual, expected)
	if not value then
		local message = 'assert.%s: expected %s, got %s'
		local formatted = message:format(assertType, tostring(expected), tostring(actual))
		error(formatted, 3)
	end
end

local kAssertHelpers = {
	throws = function(pattern, func, ...)
		local success, err = pcall(func, ...)
		if not success and err:find(pattern) then
			return
		end
		if success then
			err = 'no error'
		end
		formattedAssert(not success, 'throws', err, 'error matching "' .. pattern .. '"')
	end,
	notThrows = function(func, ...)
		local success, err = pcall(func, ...)
		formattedAssert(success, 'notThrows', err, 'no error')
	end,
	yes = function(value)
		formattedAssert(value, 'yes', value, 'truthy')
	end,
	no = function(value)
		formattedAssert(not value, 'no', value, 'falsey')
	end,
	eq = function(actual, expected)
		formattedAssert(equals(actual, expected), 'eq', actual, expected)
	end,
	deepEq = function(actual, expected)
		formattedAssert(deepEquals(actual, expected), 'deepEq', actual, expected)
	end,
	notEq = function(actual, expected)
		formattedAssert(not equals(actual, expected), 'notEq', actual, expected)
	end,
	notDeepEq = function(actual, expected)
		formattedAssert(not deepEquals(actual, expected), 'notDeepEq', actual, expected)
	end,
}

function addon.registerTest(addonName, func, test)
	testsByAddon[addonName] = testsByAddon[addonName] or {}
	table.insert(testsByAddon[addonName], { func = func, test = test })
end

function addon.runTests(addonName)
	local tests = testsByAddon[addonName]
	local errors = {}
	for _, funcAndTest in ipairs(tests) do
		local success, err = pcall(funcAndTest.test, funcAndTest.func, kAssertHelpers)
		if not success then
			table.insert(errors, err)
		end
	end
	local report = '>> Test report for %s - %d / %d passed'
	print(report:format(addonName, #tests - #errors, #tests))
	for i, err in ipairs(errors) do
		local relevant = err:match('^Interface\\AddOns\\' .. addonName .. '\\(.+)')
		print(('%d. %s'):format(i, relevant or err))
	end
end
