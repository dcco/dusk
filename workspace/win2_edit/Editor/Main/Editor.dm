
struct Editor{
	GameRoom room,
	TilePal pal,
	Int mode,
	(Int, Int) prevPos
}

fn newEditor() Editor
	return new Editor{
		room = newRoom(),
		pal = newTilePal(),
		mode = 0,
		prevPos = (0, 0)
	}
end

fn paint(GameRoom room, (Int, Int) prev, (Int, Int) cur, TType v)
	var (px, py) = prev
	var (cx, cy) = cur
	if !room.contains(px, py) || !room.contains(cx, cy) then return end
	if abs(px - cx) > 1 || abs(py - cy) > 1 then
		-- slope < 1
		if abs(px - cx) > abs(py - cy) then
			var sx = min(px, cx)
			var sy = py
			if cx < px then sy = cy end
			for i < abs(px - cx) do
				var tx = sx + i
				var ty = sy + floor(i * (py - cy) /. (px - cx))
				room.setTile(tx, ty, v)
			end
		-- slope > 1
		else
			var sy = min(py, cy)
			var sx = px
			if cy < py then sx = cx end
			for j < abs(py - cy) do
				var ty = sy + j
				var tx = sx + floor(j * (px - cx) /. (py - cy))
				room.setTile(tx, ty, v)
			end
		end
	else
		room.setTile(cx, cy, v)
	end
end

fn update(Editor editor)
	var room = editor.room
	var (mx, my) = Input.mousePos()
	var tx = mx / _TSIZE / canvasZoom()
	var ty = my / _TSIZE / canvasZoom()
	if Input.mouseDown(^left) then
		var v = OldGrass
		if editor.mode = 1 then v = BrickGrass end
		room.paint(editor.prevPos, (tx, ty), v)
	elsif Input.mouseDown(^right) then
		room.paint(editor.prevPos, (tx, ty), NullTile)
	end
	if Input.keyPress(^z) then
		editor.mode = 1 - editor.mode
	end
	editor.prevPos = (tx, ty)
end

fn draw(Editor editor)
	editor.room.draw()
	editor.pal.draw()
end