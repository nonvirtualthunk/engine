package arx.core.vec

/**
 * Created with IntelliJ IDEA.
 * User: nvt
 * Date: 12/5/15
 * Time: 11:06 AM
 */

import arx.Prelude._
import scalaxy.loops._

object Cardinals {
	val Left = 0
	val Back = 1
	val Bottom = 2
	val Right = 3
	val Front = 4
	val Top = 5
	val Center = 6

	val Up = Top
	val Down = Bottom


	val UnitNX = ReadVec3f(-1.0f,0.0f,0.0f)
	val UnitNY = ReadVec3f(0.0f,-1.0f,0.0f)
	val UnitNZ = ReadVec3f(0.0f,0.0f,-1.0f)

	//  LEFT        BACK         BOTTOM        RIGHT         FRONT           TOP
	val XAxes = Array(UnitNX,		UnitNY		,UnitNZ		,Vec3f.UnitX	,Vec3f.UnitY	,Vec3f.UnitZ)
	val YAxes = Array(UnitNY,		Vec3f.UnitX	,Vec3f.UnitY,Vec3f.UnitY	,UnitNX			,UnitNY)
	val ZAxes = Array(Vec3f.UnitZ,Vec3f.UnitZ	,Vec3f.UnitX,Vec3f.UnitZ	,Vec3f.UnitZ	,UnitNX)

	val dirvec = Array[Vec3i](Vec3i(-1,0,0),Vec3i(0,-1,0),Vec3i(0,0,-1),Vec3i(1,0,0),Vec3i(0,1,0),Vec3i(0,0,1))
	val halfDirvec = Array[Vec3f](Vec3f(-0.5f,0,0),Vec3f(0,-0.5f,0),Vec3f(0,0,-0.5f),Vec3f(0.5f,0,0),Vec3f(0,0.5f,0),Vec3f(0,0,0.5f))

	val dirvec2d = Array[Vec3i](Vec3i(-1,0,0),Vec3i(0,-1,0),Vec3i(1,0,0),Vec3i(0,1,0))

	val distanceBetweenDirections = for (i <- 0 until 7) yield { for(j <- 0 until 7) yield {
		if ( i == Center || j == Center ) { 0 }
		else if ( j == (i + 3)%6 ) { 2 }
		else if ( j == i ) { 0 }
		else { 1 }
	} }
	def distanceBetweenDirections(i:Int,j:Int) : Int = distanceBetweenDirections(i)(j)
	val oppositeDirection = for ( i <- 0 until 7 ) yield { if ( i == Center ) { Center } else { (i + 3)%6 } }

	val cardinalsX = Array(-1,0,0,1,0,0,0)
	val cardinalsY = Array(0,-1,0,0,1,0,0)
	val cardinalsZ = Array(0,0,-1,0,0,1,0)

	val oppositeSideIndices = Array(Right,Front,Top,Left,Back,Bottom,Center)

	val Forward = Front
	val Backward = Back

	val cardinals : Array[ReadVec3i] = Array[ReadVec3i](Vec3i(-1,0,0),Vec3i(0,-1,0),Vec3i(0,0,-1),Vec3i(1,0,0),Vec3i(0,1,0),Vec3i(0,0,1))
	val expandedCardinals : Array[ReadVec3i] = Array[ReadVec3i](	Vec3i(-1,0,0),Vec3i(0,-1,0),Vec3i(0,0,-1),Vec3i(1,0,0),Vec3i(0,1,0),Vec3i(0,0,1),
		Vec3i(-1,0,1),Vec3i(0,-1,1),Vec3i(1,0,1),Vec3i(0,1,1),
		Vec3i(-1,0,-1),Vec3i(0,-1,-1),Vec3i(1,0,-1),Vec3i(0,1,-1),
		Vec3i(-1,-1,0),Vec3i(-1,1,0),Vec3i(1,-1,0),Vec3i(1,1,0))
	val expandedCardinalLength = expandedCardinals.map( _.length )
	val fullCardinals = Array[ReadVec3i](	Vec3i(-1,0,0),Vec3i(0,-1,0),Vec3i(0,0,-1),Vec3i(1,0,0),Vec3i(0,1,0),Vec3i(0,0,1),
		Vec3i(-1,0,1),Vec3i(0,-1,1),Vec3i(1,0,1),Vec3i(0,1,1),
		Vec3i(-1,0,-1),Vec3i(0,-1,-1),Vec3i(1,0,-1),Vec3i(0,1,-1),
		Vec3i(-1,-1,-1),Vec3i(-1,1,-1),Vec3i(1,-1,-1),Vec3i(1,1,-1),
		Vec3i(-1,-1,1),Vec3i(-1,1,1),Vec3i(1,-1,1),Vec3i(1,1,1))
	val ExpandedCenter = 18


	val expandedCardinalsX = expandedCardinals.map( _.x ).toArray
	val expandedCardinalsY = expandedCardinals.map( _.y ).toArray
	val expandedCardinalsZ = expandedCardinals.map( _.z ).toArray

	val expandedOppositeSideIndices = (for ( i <- 0 to 18 ) yield {
		if ( i == 18 ) { ExpandedCenter }
		else {
			val base = expandedCardinals(i)
			((0 until 18) find { j => expandedCardinals(j) == base * -1 }).get
		}
	}).toArray

	val low = 0.0f
	val high = 1.0f
	/* q,k order */
	val cubePoints = Array[Array[ReadVec3f]](
		Array[ReadVec3f]( ReadVec3f(low,high,high) , ReadVec3f(low,low,high)  , ReadVec3f(low,low,low), ReadVec3f(low,high,low)  ).reverse,  //Left
		Array[ReadVec3f]( ReadVec3f(low,low,high) , ReadVec3f(high,low,high)  , ReadVec3f(high,low,low), ReadVec3f(low,low,low) ).reverse,   //Back
		Array[ReadVec3f]( ReadVec3f(high,high,low) , ReadVec3f(low,high,low)  , ReadVec3f(low,low,low), ReadVec3f(high,low,low)  ).reverse,  //Bottom
		Array[ReadVec3f]( ReadVec3f(high,low,high) , ReadVec3f(high,high,high), ReadVec3f(high,high,low), ReadVec3f(high,low,low)).reverse,  //Right
		Array[ReadVec3f]( ReadVec3f(high,high,high) , ReadVec3f(low,high,high) , ReadVec3f(low,high,low), ReadVec3f(high,high,low) ).reverse,//Front
		Array[ReadVec3f]( ReadVec3f(low,high,high), ReadVec3f(high,high,high) , ReadVec3f(high,low,high) , ReadVec3f(low,low,high) ).reverse //Top
	)

	val lowf = -0.500001f
	val highf = 0.500001f

	val centeredCubePointsFractional = Array[Array[ReadVec3f]](
		Array[ReadVec3f]( ReadVec3f(lowf,highf,highf) , ReadVec3f(lowf,lowf,highf)  , ReadVec3f(lowf,lowf,lowf), ReadVec3f(lowf,highf,lowf)  ).reverse,  //Left
		Array[ReadVec3f]( ReadVec3f(lowf,lowf,highf) , ReadVec3f(highf,lowf,highf)  , ReadVec3f(highf,lowf,lowf), ReadVec3f(lowf,lowf,lowf) ).reverse,   //Back
		Array[ReadVec3f]( ReadVec3f(highf,highf,lowf) , ReadVec3f(lowf,highf,lowf)  , ReadVec3f(lowf,lowf,lowf), ReadVec3f(highf,lowf,lowf)  ).reverse,  //Bottom
		Array[ReadVec3f]( ReadVec3f(highf,lowf,highf) , ReadVec3f(highf,highf,highf), ReadVec3f(highf,highf,lowf), ReadVec3f(highf,lowf,lowf)).reverse,  //Right
		Array[ReadVec3f]( ReadVec3f(highf,highf,highf) , ReadVec3f(lowf,highf,highf) , ReadVec3f(lowf,highf,lowf), ReadVec3f(highf,highf,lowf) ).reverse,//Front
		Array[ReadVec3f]( ReadVec3f(lowf,highf,highf), ReadVec3f(highf,highf,highf) , ReadVec3f(highf,lowf,highf) , ReadVec3f(lowf,lowf,highf) ).reverse //Top
	)

	val centeredCubeCenterPointsFractional = Array[ReadVec3f](
		Vec3f(lowf,(lowf+highf)*0.5f,(lowf+highf)*0.5f),	//Left
		Vec3f((lowf+highf)*0.5f,lowf,(lowf+highf)*0.5f),	//Back
		Vec3f((lowf+highf)*0.5f,(lowf+highf)*0.5f,lowf),	//Bottom
		Vec3f(highf,(lowf+highf)*0.5f,(lowf+highf)*0.5f),	//Right
		Vec3f((lowf+highf)*0.5f,highf,(lowf+highf)*0.5f),	//Front
		Vec3f((lowf+highf)*0.5f,(lowf+highf)*0.5f,highf)		//Top
	)

	val cubePointsFlat = Array.ofDim[ReadVec3f](24)
	for ( k <- 0 until 4 ; q <- 0 until 6 ) { cubePointsFlat((q << 2) + k) = Vec3f(cubePoints(q)(k)) }

	val cubePointsi = Array[Array[ReadVec3i]](
		Array[ReadVec3i]( ReadVec3i(0,1,1) , ReadVec3i(0,0,1) , ReadVec3i(0,0,0), ReadVec3i(0,1,0) ).reverse,
		Array[ReadVec3i]( ReadVec3i(0,0,1) , ReadVec3i(1,0,1)	, ReadVec3i(1,0,0), ReadVec3i(0,0,0) ).reverse,
		Array[ReadVec3i]( ReadVec3i(1,1,0) , ReadVec3i(0,1,0) , ReadVec3i(0,0,0), ReadVec3i(1,0,0) ).reverse,
		Array[ReadVec3i]( ReadVec3i(1,0,1) , ReadVec3i(1,1,1) , ReadVec3i(1,1,0), ReadVec3i(1,0,0) ).reverse,
		Array[ReadVec3i]( ReadVec3i(1,1,1) , ReadVec3i(0,1,1) , ReadVec3i(0,1,0), ReadVec3i(1,1,0) ).reverse,
		Array[ReadVec3i]( ReadVec3i(0,1,1), ReadVec3i(1,1,1) , ReadVec3i(1,0,1) , ReadVec3i(0,0,1) ).reverse
	)

	val cubePointsFlati = Array.ofDim[ReadVec3i](24)
	for ( k <- 0 until 4 ; q <- 0 until 6 ) { cubePointsFlati((q << 2) + k) = Vec3i(cubePointsi(q)(k)) }

	val centeredCubePoints = Array[Array[ReadVec3f]](
		Array[ReadVec3f]( ReadVec3f(-0.5f,0.5f,0.5f) , ReadVec3f(-0.5f,-0.5f,0.5f) , ReadVec3f(-0.5f,-0.5f,-0.5f), ReadVec3f(-0.5f,0.5f,-0.5f) ).reverse,
		Array[ReadVec3f]( ReadVec3f(-0.5f,-0.5f,0.5f) , ReadVec3f(0.5f,-0.5f,0.5f) , ReadVec3f(0.5f,-0.5f,-0.5f), ReadVec3f(-0.5f,-0.5f,-0.5f) ).reverse,
		Array[ReadVec3f]( ReadVec3f(0.5f,0.5f,-0.5f) , ReadVec3f(-0.5f,0.5f,-0.5f) , ReadVec3f(-0.5f,-0.5f,-0.5f), ReadVec3f(0.5f,-0.5f,-0.5f) ).reverse,
		Array[ReadVec3f]( ReadVec3f(0.5f,-0.5f,0.5f) , ReadVec3f(0.5f,0.5f,0.5f)   , ReadVec3f(0.5f,0.5f,-0.5f), ReadVec3f(0.5f,-0.5f,-0.5f)   ).reverse,
		Array[ReadVec3f]( ReadVec3f(0.5f,0.5f,0.5f) , ReadVec3f(-0.5f,0.5f,0.5f)   , ReadVec3f(-0.5f,0.5f,-0.5f), ReadVec3f(0.5f,0.5f,-0.5f)   ).reverse,
		Array[ReadVec3f]( ReadVec3f(-0.5f,0.5f,0.5f), ReadVec3f(0.5f,0.5f,0.5f)     , ReadVec3f(0.5f,-0.5f,0.5f) , ReadVec3f(-0.5f,-0.5f,0.5f) ).reverse )
	val cubeOrthoIndices = Array[Array[ReadVec2i]](
		Array[ReadVec2i]( ReadVec2i(Front,Up) , ReadVec2i(Back,Up)			, ReadVec2i(Back,Down) , ReadVec2i(Front,Down)     ).reverse,
		Array[ReadVec2i]( ReadVec2i(Left,Up) , ReadVec2i(Right,Up)				,  ReadVec2i(Right,Down) , ReadVec2i(Left,Down)      ).reverse,
		Array[ReadVec2i]( ReadVec2i(Right,Forward) , ReadVec2i(Left,Front)	, ReadVec2i(Left,Back) , ReadVec2i(Right,Back)       ).reverse,
		Array[ReadVec2i]( ReadVec2i(Backward,Up) , ReadVec2i(Front,Up)		, ReadVec2i(Front,Down) , ReadVec2i(Back,Down) ).reverse,
		Array[ReadVec2i]( ReadVec2i(Right,Up) , ReadVec2i(Left,Up)				, ReadVec2i(Left,Down) , ReadVec2i(Right,Down)			).reverse,
		Array[ReadVec2i]( ReadVec2i(Left,Forward) , ReadVec2i(Right,Forward) , ReadVec2i(Right,Backward) , ReadVec2i(Left,Backward) ).reverse
	)
	val cubeOrthos = Array.ofDim[ReadVec3i](6,4,2)
	for ( i <- 0 until 6 ; j <- 0 until 4 ) {
		cubeOrthos(i)(j)(0) = cardinals( cubeOrthoIndices(i)(j).x )
		cubeOrthos(i)(j)(1) = cardinals( cubeOrthoIndices(i)(j).y )
	}

	val cubeOrthosf = Array.ofDim[ReadVec3f](6,4,2)
	for ( i <- 0 until 6 ; j <- 0 until 4 ) {
		cubeOrthosf(i)(j)(0) = cardinals( cubeOrthoIndices(i)(j).x )
		cubeOrthosf(i)(j)(1) = cardinals( cubeOrthoIndices(i)(j).y )
	}

	val cubeHalfOrthos = Array.ofDim[ReadVec3f](6,4,2)
	for ( i <- 0 until 6 ; j <- 0 until 4 ) {
		cubeHalfOrthos(i)(j)(0) = cardinals( cubeOrthoIndices(i)(j).x ) * 0.5f
		cubeHalfOrthos(i)(j)(1) = cardinals( cubeOrthoIndices(i)(j).y ) * 0.5f
	}

	val edgeCorners = Array( 	(Vec3i(0,0,0),Vec3i(0,1,1)) , //left
		(Vec3i(0,0,0),Vec3i(1,0,1)) , //back
		(Vec3i(0,0,0),Vec3i(1,1,0)) , //bottom
		(Vec3i(1,0,0),Vec3i(1,1,1)) , //right
		(Vec3i(0,1,0),Vec3i(1,1,1)) , //front
		(Vec3i(0,0,1),Vec3i(1,1,1)) //top
	);
	val cubeFaceNormals = Array[Vec3f]( Vec3f(-1.0f,0.0f,0.0f) , Vec3f(0.0f,-1.0f,0.0f) , Vec3f(0.0f,0.0f,-1.0f) ,
		Vec3f(1.0f,0.0f,0.0f)  , Vec3f(0.0f,1.0f,0.0f)  , Vec3f(0.0f,0.0f,1.0f) )

	val unitBillboardOffsets = Array(Vec3f(-0.5f,0.0f,0.0f),Vec3f(0.5f,0.0f,0.0f),Vec3f(0.5f,1.0f,0.0f),Vec3f(-0.5f,1.0f,0.0f))
	val centeredBillboardOffsets = Array(Vec3f(-0.5f,-0.5f,0.0f),Vec3f(0.5f,-0.5f,0.0f),Vec3f(0.5f,0.5f,0.0f),Vec3f(-0.5f,0.5f,0.0f))

}
