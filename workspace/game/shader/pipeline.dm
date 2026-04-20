references Sys modules Sulfur

global lightShader = nullShader()

fn writeRenderList(Blob elem)
end

fn initShader()
	lightShader = newShader(cLoad("light.vs"), cLoad("light.fs"), new 1d[Int])
end

fn runShader(RenderList rl)
	lightShader.render(rl)
end