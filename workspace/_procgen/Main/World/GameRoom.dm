
struct GameRoom{
	Int width,
	Int height,
	2d[GameTile] grid
}

fn draw(GameRoom room)
	for i < room.width, j < room.height do
		room.grid[i, j].draw(i, j)
	end
end