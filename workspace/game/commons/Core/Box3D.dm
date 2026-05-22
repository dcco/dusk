
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

fn centerX(Box3D self) Int
	return self.x + self.width / 2
end

fn centerZ(Box3D self) Int
	return self.z + self.depth / 2
end

