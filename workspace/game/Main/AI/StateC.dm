
enum AType = NoAnim | IdleAnim

	(*
		motion: continuous behavior performed as part of a state
			returns the type of animation to use
	*)

fn noMotion(GameObj obj) AType
	return NoAnim
end

fn pcMotion(GameObj obj) AType
	return IdleAnim
end

(*
enum MotionC attrs{ Fn(GameObj) AType update } =
	NoMotion{ update = 0 }
	| PCMotion{ update = 0 }*)

	(*
		state: composable wrappers over motions + actions, defining total object behavior
			generally a sequence of motions w/ end conditions, followed by some looping/termination action

		- SimpS: loops one motion forever
		- LoopS: repeats a list of states forever
		- SeqS: executes a list of states, repeats final state forever
		- BranchS: executes a list of states, branches to new state based on conditional
		- SMS: special branch, branches based on list of probabilities
		- CallbackS: executes a list of states, returns to a special "callback" state when finished
		- PCS: special player character state
	*)
(*
enum StateC attrs{ Fn() MotionC motion } =
	PCState{ motion = 0 }*)