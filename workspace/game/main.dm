references Sys modules Os, Input, Sulfur, Rom

const pieces = new 3d[4, 2, 7][
	0, 0, 0, 0,
	1, 1, 1, 1,

	0, 1, 1, 0,
	0, 0, 1, 1,
	
	0, 1, 1, 0,
	1, 1, 0, 0,
	
	0, 1, 0, 0,
	1, 1, 1, 0,
	
	0, 1, 1, 0,
	0, 1, 1, 0,

	0, 1, 0, 0,
	0, 1, 1, 1,

	0, 0, 1, 0,
	1, 1, 1, 0
]

struct Piece{
	Int type,
	Int x,
	Int y
}

struct Game{
	Piece cur,
	2d[Int] board
}

fn initGame() Game
	var grid = new 2d[1, 1][0]
	(*for i < 10, j < 20 do
		if randomInt(2) < 1 then
			grid[i, j] = 0
		else
			grid[i, j] = 1
		end
	end
	for i < 4, j < 2 do
		grid[i, j] = pieces[i, j, 3]
	end*)
	return new Game{
		cur = new Piece{ type = 0, x = 4, y = 0},
		board = grid
	}
end

fn draw(Game game)
	for i < 10, j < 20 do
		if game.board[i, j] = 1 then
			Sulfur.draw(Sprite(i * 8, j * 8, tset, 2))
		end
	end
end

lin main()
	-- build map
	var game = initGame()
	-- timing
	var fps = 60L
	var nsPerFrame = 1000000000L / fps
	var prevTime = Os.time()
	loop
		var curTime = Os.time()
		var framesPassed = toInt((curTime - prevTime) / nsPerFrame)
		(*for i < framesPassed do
		end*)
		if framesPassed >= 1 then
			-- draw map
			draw(game)
			-- finish
			Sulfur.refresh()
			gc_collect
			prevTime = prevTime + (toLong(framesPassed) * nsPerFrame)
		end
	end
end