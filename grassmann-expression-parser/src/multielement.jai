#scope_export

// In the AST we have nodes representing individual K_Elements in the space.
// When we evaluate the AST the final expression will either be a single k-element (e1),
// a sum of k-elements of the same grade (e1+e2+e3) or a sum of elements of different grades
// (e1 + e1e2 + e1e2e3).
//
// The last case is called a 'Multivector' in Clifford Algebras and subsumes the previous other types of results.
//
// Since all examples above are considered Multivectors we have one single type, the MultiElement, which will be used
// for the output. The reason we use 'Element' instead of 'Vector' is that Grassmann allowed the mixing of point-spaces
// with vector spaces. In a point-space there is an object representing the origin which is not technically a vector.
//
// Therefore, to be more general we call each object in the space an element.

K_Element :: struct {
	// matches definition in ast.jai of K_Element_Node	
	grade: int;
	coefficient: int;
	one_element_components: s64;
}

Multi_Element :: struct {

	elements: [..] K_Element;
}
