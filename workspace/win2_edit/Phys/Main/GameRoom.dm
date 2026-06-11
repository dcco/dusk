
struct GameRoom{
	2d[GameTile] data
}

fn newRoom() GameRoom
	var data = new 2d(40, 40)[..
		new GameTile{
			cType = NullTile,
			rFrame = 0
		}
	]
	return new GameRoom{
		data = data
	}
end

fn contains(GameRoom room, Int tx, Int ty) Bool
	var (w, h) = |room.data|
	return tx >= 0 && tx < w && ty >= 0 && ty < h
end

fn getTile(GameRoom room, Int tx, Int ty) TType
	if !room.contains(tx, ty) then return NullTile end
	return room.data[tx, ty].cType
end

fn reify(GameRoom room, Int tx, Int ty)
	if !room.contains(tx, ty) then return end
	if room.data[tx, ty].cType is NullTile then return end
	var a1 = room.getTile(tx - 1, ty - 1) isnt NullTile
	var a2 = room.getTile(tx, ty - 1) isnt NullTile
	var a3 = room.getTile(tx + 1, ty - 1) isnt NullTile
	var b1 = room.getTile(tx - 1, ty) isnt NullTile
	var b3 = room.getTile(tx + 1, ty) isnt NullTile
	var c1 = room.getTile(tx - 1, ty + 1) isnt NullTile
	var c2 = room.getTile(tx, ty + 1) isnt NullTile
	var c3 = room.getTile(tx + 1, ty + 1) isnt NullTile
	room.data[tx, ty].rFrame = 0
	var rt = room.data[tx, ty].cType.rt
	if rt is NoReify then return end
	if !a2 && c2 then
		if !b1 && b3 && c3 then room.data[tx, ty].rFrame = 1
		elsif !b3 && b1 && c1 then room.data[tx, ty].rFrame = 3
		elsif b1 && b3 && (c1 || c3) then room.data[tx, ty].rFrame = 2 end
	elsif a2 then
		if !b1 && b3 && (a3 || (c2 && c3)) then room.data[tx, ty].rFrame = 4
		elsif !b3 && b1 && (a1 || (c2 && c1)) then room.data[tx, ty].rFrame = 6
		elsif b1 && b3 && (a1 || a3 || (c2 && c3) || (c2 && c1)) then room.data[tx, ty].rFrame = 5 end
		-- reify bottom tile when appropriate
		if !c2 && rt is FullReify && room.data[tx, ty].rFrame >= 4 && room.data[tx, ty].rFrame <= 6 then
			room.data[tx, ty].rFrame = room.data[tx, ty].rFrame + 3
		end
	end 
end

fn setTile(GameRoom room, Int tx, Int ty, TType v)
	room.data[tx, ty].cType = v
	for i < 3, j < 3 do
		room.reify(tx + i - 1, ty + j - 1)
	end
end

fn draw(GameRoom room)
	var (w, h) = |room.data|
	for i < w, j < h do
		room.data[i, j].draw(i * _TSIZE, j * _TSIZE)
	end 
end