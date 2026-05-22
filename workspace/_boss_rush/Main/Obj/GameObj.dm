
struct GameObj{
	MoveObj body,
	Bool facing
}

fn newGameObj() GameObj
	return new GameObj{
		body = newMoveObj(8, 8),
		facing = false
	}
end

fn draw(GameObj obj)
	var fx = toFP(obj.body.hitbox.x)
	var fy = toFP(obj.body.hitbox.y)
	Sulfur.drawSprite(fx, fy, 0.5, heidi, 1, obj.facing)
end