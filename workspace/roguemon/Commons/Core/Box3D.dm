
struct Box3D{
	Int x,
	Int y,
	Int z,
	Int width,
	Int height,
	Int depth
}

fn newBox3D(Int w, Int h, Int d) Box3D
	return new Box3D{
		x = 0, y = 0, z = 0,
		width = w, height = h, depth = d
	}
end

fn bottom(Box3D self) Int
	return self.y + self.height
end

fn centerX(Box3D self) Int
	return self.x + self.width / 2
end

fn centerY(Box3D self) Int
	return self.y + self.height / 2
end

fn centerZ(Box3D self) Int
	return self.z + self.depth / 2
end

fn overlaps(Box3D self, Box3D other) Bool
	return self.x + self.width > other.x && self.x < other.x + other.width &&
		self.y + self.height > other.y && self.y < other.y + other.height &&
		self.z + self.depth > other.z && self.z < other.z + other.depth
end

fn rangeI(Box3D self, Int axis) (Int, Int)
	if axis = 0 then return (self.x, self.x + self.width)
	elsif axis = 1 then return (self.y, self.y + self.height)
	else return (self.z, self.z + self.depth) end
end

fn chxRangeH(Box3D self, Int axis, (Int, Int) r) (Int, Int, Int, Int)
	if axis = 0 then return (r.1, r.2, self.z, self.z + self.depth) end
	return (self.x, self.x + self.width, r.1, r.2)
end