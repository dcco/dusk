references Sys modules Os, Input, Sulfur, Rom

	(*
		tetr~s pieces
	*)

const pieces = new 3d[4, 2, 7][
	1, 1, 1, 1,
	0, 0, 0, 0,

	1, 1, 0, 0,
	0, 1, 1, 0,
	
	0, 1, 1, 0,
	1, 1, 0, 0,
	
	0, 1, 0, 0,
	1, 1, 1, 0,
	
	0, 1, 1, 0,
	0, 1, 1, 0,

	1, 0, 0, 0,
	1, 1, 1, 0,

	0, 0, 1, 0,
	1, 1, 1, 0
]

struct Piece{
	Int type,
	Int orient,
	Int x,
	Int y
}

fn colData(Int type, Int orient) 2d[Int]
	var data = new 2d[4, 4][
		0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0,
		0, 0, 0, 0
	]
	-- var data = new 2d(4, 4)[.. 0]
	for i < 4, j < 2 do
		if orient = 1 then
			data[2 - j, i] = pieces[i, j, type]
		elsif orient = 2 then
			data[3 - i, 2 - j] = pieces[i, j, type]
		elsif orient = 3 then
			data[j + 1, 3 - i] = pieces[i, j, type]
		else
			data[i, j + 1] = pieces[i, j, type]
		end
	end
	return data
end

	(*
		main game state
	*)

struct Game{
	Int clock,
	Piece curPiece,
	2d[Int] board
}

fn initGame() Game
	var grid = new 2d(10, 20)[.. 0]
	return new Game{
		clock = 0,
		curPiece = new Piece{ type = randomInt(7), orient = 0, x = 3, y = -1 },
		board = grid
	}
end

fn isSolid(Game game, Int x, Int y) Bool
	if x < 0 || x >= 10 || y >= 20 then return true
	elsif y < 0 then return false end
	return game.board[x, y] != 0
end

fn tryMove(Game game, Int x, Int y) Bool
	var piece = game.curPiece
	var cData = colData(piece.type, piece.orient)
	for i < 4, j < 4 do
		if cData[i, j] = 1 && game.isSolid(x + i, y + j) then
			return false
		end
	end
	piece.x = x
	piece.y = y
	return true
end

fn tryOrient(Game game, Int orient, Int offX, Int offY) Bool
	var piece = game.curPiece
	var cData = colData(piece.type, orient)
	for i < 4, j < 4 do
		if cData[i, j] = 1 && game.isSolid(piece.x + i + offX, piece.y + j + offY) then
			return false
		end
	end
	piece.orient = orient
	piece.x = piece.x + offX
	piece.y = piece.y + offY
	return true
end

fn clearLine(Game game, Int y)
	for i < 10, j < y - 1 do
		var jx = y - j
		game.board[i, jx] = game.board[i, jx - 1]
	end
end

fn clearLines(Game game)
	for j < 20 do
		var complete = true
		for i < 10 do
			if game.board[i, j] = 0 then complete = false end
		end
		if complete then
			clearLine(game, j)
		end
	end
end

fn dropPiece(Game game)
	var piece = game.curPiece
	var cData = colData(piece.type, piece.orient)
	for i < 4, j < 4 do
		var y = j + piece.y
		if y >= 0 && cData[i, j] != 0 then
			game.board[piece.x + i, y] = piece.type + 1
		end
	end
	game.curPiece = new Piece{ type = randomInt(7), orient = 0, x = 3, y = -1 }
	game.clearLines()
end

fn update(Game game)
	-- piece fall
	var piece = game.curPiece
	game.clock = game.clock + 1
	if game.clock >= 55 then
		var falling = game.tryMove(piece.x, piece.y + 1)
		if !falling then game.dropPiece() end
		game.clock = 0
	end
	-- piece movement
	if Input.keyPress(^left) then
		game.tryMove(piece.x - 1, piece.y)
	elsif Input.keyPress(^right) then
		game.tryMove(piece.x + 1, piece.y)
	elsif Input.keyPress(^down) then
		game.tryMove(piece.x, piece.y + 1)
	end
	-- piece rotation
	if Input.keyPress(^z) then
		var newOrient = (piece.orient + 3) % 4
		var good = game.tryOrient(newOrient, 0, 0)
		if !good then good = game.tryOrient(newOrient, 1, 0) end
		if !good then game.tryOrient(newOrient, -1, 0) end
	elsif Input.keyPress(^x) then
		var newOrient = (piece.orient + 1) % 4
		var good = game.tryOrient(newOrient, 0, 0)
		if !good then good = game.tryOrient(newOrient, -1, 0) end
		if !good then game.tryOrient(newOrient, 1, 0) end
	end
	-- piece hard drop
	if Input.keyPress(^up) then
		var falling = true
		while falling do
			falling = game.tryMove(piece.x, piece.y + 1)
		end
		game.dropPiece()
		game.clock = 0
	end
end

fn draw(Game game)
	for i < 10, j < 20 do
		Sulfur.draw(Sprite(i * 8 + 64, j * 8 + 32, tset, game.board[i, j] + 1))
	end
	var piece = game.curPiece
	var cData = colData(piece.type, piece.orient)
	for i < 4, j < 4 do
		var px = piece.x + i
		var py = piece.y + j
		if cData[i, j] = 1 then
			Sulfur.draw(Sprite(px * 8 + 64, py * 8 + 32, tset, piece.type + 2))
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
		for i < framesPassed do
			Input.update()
			game.update()
		end
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