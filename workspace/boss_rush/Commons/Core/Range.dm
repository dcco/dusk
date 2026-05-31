
fn extend((Int, Int) r, Int delta) (Int, Int)
	if delta < 0 then return (r.1 + delta, r.1) end
	return (r.2, r.2 + delta)
end

fn diff((Int, Int) self, (Int, Int) other, Int dir) (Int, Int)
	if dir < 0 && self.1 < other.2 then return (other.2, self.2)
	elsif dir > 0 && self.2 > other.1 then return (self.1, other.1) end
	return (self.1, self.2)
end