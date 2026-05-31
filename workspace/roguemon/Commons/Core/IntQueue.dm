
struct IntQueue{
	1d[Int] addStack,
	1d[Int] remStack
}

fn newIntQueue() IntQueue
	return new IntQueue{
		addStack = new 1d[~Int],
		remStack = new 1d[~Int]
	}
end

fn empty(IntQueue q) Bool
	return |q.addStack| = 0 && |q.remStack| = 0
end

fn add(IntQueue q, Int i)
	q.addStack += i
end

fn remove(IntQueue q) Int
	if |q.remStack| = 0 then
		if |q.addStack| = 0 then return -1 end
		while |q.addStack| > 0 do
			var z = q.addStack[|q.addStack| - 1]
			q.addStack /= |q.addStack| - 1
			q.remStack += z
		end
	end
	var f = q.remStack[|q.remStack| - 1]
	q.remStack /= |q.remStack| - 1
	return f
end