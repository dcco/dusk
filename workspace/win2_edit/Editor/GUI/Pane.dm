
struct Pane{
	Int x,
	Int y,
	Int width,
	Int height,
	2d[Pane] childList,
	Fn(Pane, Int, Int) drawFun
}
