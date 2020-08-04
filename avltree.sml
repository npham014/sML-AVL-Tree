datatype 'a baltree = 
	EmptyTree
	| TwoNode of 'a * 'a baltree * 'a baltree
	| ThreeNode of 'a * 'a * 'a baltree * 'a baltree * 'a baltree
	| MovingNode of 'a * 'a baltree * 'a baltree; (*This is to assist with insert. I tried to do it with an entirely new datatype but my implementation would have needed multiple return types*)

fun find23 comparator EmptyTree searchTerm = NONE
	| find23 comparator (TwoNode(a,b,c)) searchTerm = if comparator (searchTerm,a) = 0 then SOME a else
		if comparator (searchTerm,a) = ~1 then find23 comparator b searchTerm else find23 comparator c searchTerm
	| find23 comparator (ThreeNode(root1,root2,left,middle,right)) searchTerm = 
		let
			val minComp = comparator (searchTerm,root1)
			val maxComp = comparator (searchTerm,root2)
		in
			if minComp = 0 then SOME root1 else
				if minComp = ~1 then find23 comparator left searchTerm else
					if maxComp = 0 then SOME root2 else
						if maxComp = 1 then find23 comparator right searchTerm else
							find23 comparator middle searchTerm
		end;

(*Insert helper function to add the extra datatype of baltree to the node above. This one is for adding a new subtree to a twonode above: just change to three node*)
(*Use pattern matching to find out whether the new val goes on left or right*)
fun addToTwoNode (TwoNode(root,MovingNode(mRoot,mleft,mright),right)) = ThreeNode(mRoot,root,mleft,mright,right)
	| addToTwoNode (TwoNode(root,left,MovingNode(mRoot,mleft,mright))) = ThreeNode(root,mRoot,left,mleft,mright)
	| addToTwoNode (TwoNode(root,left,right)) = TwoNode(root,left,right);(*It's possible(probable even) that this gets called on something that isn't supposed to be moved to its parent. In this case dont do anything*)

(*Insert helper function to add the extra datatype of baltree to the node above. This one is to add to nodes that already have two values and change it into a two node with two subtrees. To keep height, these themselves must also be moving nodes*)
fun addToThreeNode (ThreeNode(leftRoot,rightRoot,MovingNode(mRoot,mleft,mright),middle,right)) = MovingNode(leftRoot,TwoNode(mRoot,mleft,mright),TwoNode(rightRoot,middle,right))
	|addToThreeNode (ThreeNode(leftRoot,rightRoot,left,MovingNode(mRoot,mleft,mright),right)) = MovingNode(mRoot,TwoNode(leftRoot,left,mleft),TwoNode(rightRoot,mright,right))
	|addToThreeNode (ThreeNode(leftRoot,rightRoot,left,middle,MovingNode(mRoot,mleft,mright))) = MovingNode(rightRoot,TwoNode(leftRoot,left,middle),TwoNode(mRoot,mleft,mright))
	|addToThreeNode (ThreeNode(leftRoot,rightRoot,left,middle,right)) = (ThreeNode(leftRoot,rightRoot,left,middle,right)); (*It's possible(probable even) that this gets called on something that isn't supposed to be moved to its parent. In this case dont do anything. Note: In original designs, I had to return a moving node which probably would cause errors*)

(*Main helper function of insert that does all the work. We find where the leaf should be using the comparison operator and recurse down the side that the leaf belongs to. Finally use the insert into node methods above to add to the tree*)
fun insertHelper comp EmptyTree newVal = TwoNode(newVal,EmptyTree,EmptyTree)
	|insertHelper comp (TwoNode(root,left,right)) newVal = if comp (newVal,root) = ~1 then addToTwoNode (TwoNode(root, insertHelper comp left newVal, right)) else addToTwoNode (TwoNode(root, left , insertHelper comp right newVal)) (*Assuming that you'll never add a duplicate*)
	|insertHelper comp (ThreeNode(leftRoot,rightRoot,left,middle,right)) newVal = 
		let
			val minComp = comp (newVal,leftRoot)
			val maxComp = comp (newVal,rightRoot)
		in
			if minComp = ~1 then addToThreeNode (ThreeNode(leftRoot,rightRoot, insertHelper comp left newVal, middle, right)) else
				if maxComp = 1 then addToThreeNode (ThreeNode(leftRoot,rightRoot,left, middle, insertHelper comp right newVal))
				else addToThreeNode (ThreeNode(leftRoot,rightRoot,left, insertHelper comp middle newVal, right))
		end;

(*This is just to see if I need to fix the top node. I think I could have done this with pattern matching in the let, but I'm not sure*)
fun rootRestructure (MovingNode(root,left,right)) = true
	| rootRestructure _ = false;

(*Main insertion function. Should guarantee another programmer never sees a MovingNode.*)
fun insert23 comp tree newVal =
	let
		val finalTree = insertHelper comp tree newVal (*Call the helper function on the inserted tree*)
		val TwoNode(root,left,right) = finalTree (*Needed in case I need to fix the top of the tree*)
	in
		if rootRestructure finalTree then TwoNode(root,left,right) else finalTree
	end;


fun treedictadd comp (TwoNode((c,d), left,right)) newVal = insert23 comp (TwoNode((c,d), left,right)) newVal
	| treedictadd comp (ThreeNode((a,b),(c,d),left,mid,right)) newVal = insert23 comp (ThreeNode((a,b),(c,d),left,mid,right)) newVal
	| treedictadd comp EmptyTree newVal = insert23 comp EmptyTree newVal; (*How do I force the comparison between the keys?*)

fun treedictfind comp (TwoNode((c,d), left,right)) newVal = find23 comp (TwoNode((c,d), left,right)) newVal
	| treedictfind comp (ThreeNode((a,b),(c,d),left,mid,right)) newVal = find23 comp (ThreeNode((a,b),(c,d),left,mid,right)) newVal
	| treedictfind comp EmptyTree newVal = find23 comp EmptyTree newVal; (*How do I force the comparison between the keys?*)

