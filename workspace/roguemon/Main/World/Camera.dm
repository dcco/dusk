
struct CamState{
	Int pitchAngle,
	Float x,
	Float y,
	Float z
}

enum CamMode = ManCam | ZoomCam 

struct Camera{
	CamMode mode,
	Bool manMode,
	1d[MoveObj] focusList,
	CamState state,
	CamState target
}

fn newCamera(MoveObj focus) Camera
	var tx = toFP(focus.hitbox.centerX())
	var ty = toFP(focus.hitbox.y)
	var tz = toFP(focus.hitbox.centerZ())
	return new Camera{
		mode = ManCam,
		manMode = false,
		focusList = new 1d[focus],
		state = new CamState{
			pitchAngle = 45,
			x = tx, y = ty - 8.0, z = tz + 10.5
		},
		target = new CamState{
			pitchAngle = 45,
			x = tx, y = ty - 8.0, z = tz + 10.5
		}
	}
end

fn toggleManMode(Camera camera)
	camera.manMode = !camera.manMode
end

fn getFocus(Camera camera) (Float, Float, Float)
	var tx = 0.0
	var ty = 0.0
	var tz = 0.0
	var focusTotal = |camera.focusList|
	for i < focusTotal do
		var focus = camera.focusList[i]
		tx = (tx + toFP(focus.hitbox.centerX()))
		ty = (ty + toFP(focus.hitbox.y))
		tz = (tz + toFP(focus.hitbox.centerZ()))
	end
	if focusTotal > 1 then
		tx = (tx /. toFloat(focusTotal))
		ty = (ty /. toFloat(focusTotal))
		tz = (tz /. toFloat(focusTotal))
	end
	return (tx, ty, tz)
end

const _MABS = 0.001

fn snapTo(Int x, Int tarX) Int
	if abs(x - tarX) < 6 then
		return tarX
	end
	return x - toInt(toFloat(x - tarX) * 0.333)
end

fn snapTo(Float x, Float tarX) Float
	if abs(x - tarX) < _MABS then
		return tarX
	end
	return x - ((x - tarX) * 0.333)
end

fn update(Camera camera)
	-- set target based on mode
	var (tx, ty, tz) = camera.getFocus()
	var cMode = camera.mode
	if cMode is ZoomCam then
		camera.target.pitchAngle = 20
		camera.target.x = tx
		camera.target.y = ty - 1.2
		camera.target.z = tz + 4.5
	elsif camera.manMode then
		camera.target.pitchAngle = 40
		camera.target.x = tx
		camera.target.y = (ty - 3.8)
		camera.target.z = (tz + 5.5)
	else
		camera.target.pitchAngle = 45
		camera.target.x = tx
		camera.target.y = (ty - 8.0)
		camera.target.z = (tz + 10.5)
	end
	-- move to target
	camera.state.pitchAngle = snapTo(camera.state.pitchAngle, camera.target.pitchAngle)
	camera.state.x = snapTo(camera.state.x, camera.target.x)
	camera.state.y = snapTo(camera.state.y, camera.target.y)
	camera.state.z = snapTo(camera.state.z, camera.target.z)
end

fn updateCamMat(Int angle, Float x, Float y, Float z)
	idMat4(RV.mvMat)
	RV.mvMat.rotateX(toRadians(angle))
	RV.mvMat.translate(x, y, z)
end

fn use(Camera camera)
	var state = camera.state
	updateCamMat(-state.pitchAngle, -state.x, -state.y, -state.z)
end
