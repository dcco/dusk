
struct Rect{
	Int x,
	Int y,
	Int width,
	Int height
}

fn newRect(Int w, Int h) Rect
	return new Rect{
		x = 0, y = 0,
		width = w, height = h
	}
end

fn right(Rect self) Int
	return self.x + self.width
end

fn bottom(Rect self) Int
	return self.y + self.height
end

fn rangeX(Rect self) (Int, Int)
	return (self.x, self.x + self.width)
end

fn rangeY(Rect self) (Int, Int)
	return (self.y, self.y + self.height)
end

fn rangeI(Rect self, Int axis) (Int, Int)
	if axis = 0 then return self.rangeX() end
	return self.rangeY()
end

fn chxRange(Rect self, Int axis, (Int, Int) r) (Int, Int, Int, Int)
	if axis = 0 then return (r.1, r.2, self.y, self.y + self.height) end
	return (self.x, self.x + self.width, r.1, r.2)
end