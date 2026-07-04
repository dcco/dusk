
enum StarColor attrs { Int r, Int g, Int b } =
	WhiteStar{ 255, 255, 255 }
	| BlueStar{ 200, 235, 255 }
	| YellowStar{ 255, 240, 180 }
	| OrangeStar{ 255, 200, 140 }
	| RedStar{ 255, 150, 150 }

struct Star{
	(Int, Int) pos,
	Float brightness,
	StarColor color,
	Float twinkle,
	Int twinkPeriod,
	Float twinkPhase
}

struct SpaceMap{
	Int z
}

fn draw(SpaceMap sMap)

end