
struct PermTable{
	1d[Int] data
}

fn newPermTable(PRNG prng) PermTable
	var data = new 1d(257)[.. 0]
	for i < 256 do
		data[i] = i
	end
	for i < 256 do
		var j = i + prng.randomInt(256 - i)
		var t = data[i]
		data[i] = data[j]
		data[j] = t
	end
	data[256] = data[0]
	return new PermTable{ data = data }
end