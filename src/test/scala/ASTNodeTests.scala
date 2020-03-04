import ast._
import org.junit.Test
import org.scalatestplus.junit.JUnitSuite
import org.junit.Assert._

class ASTNodeTests extends JUnitSuite
{
	@Test def stringLiteralNode(): Unit =
	{
		val literal: StringLiteral = new StringLiteral("abc", 1)
		assertEquals(1, literal.values.length)
		assertEquals("abc", literal.values.head)
		assertEquals(Types.String, literal.nodeType)
		assertEquals("\"abc\"", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)
	}

	@Test def intLiteralNode(): Unit =
	{
		val literal: IntLiteral = new IntLiteral(42, 2)
		assertEquals(2, literal.values.length)
		assertEquals(42, literal.values.head)
		assertEquals(Types.Int, literal.nodeType)
		assertEquals("42", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)
	}

	@Test def boolLiteralNode(): Unit =
	{
		var literal: BoolLiteral = new BoolLiteral(false, 3)
		assertEquals(3, literal.values.length)
		assertEquals(false, literal.values.head)
		assertEquals(Types.Bool, literal.nodeType)
		assertEquals("False", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)

		literal = new BoolLiteral(true, 4)
		assertEquals(4, literal.values.length)
		assertEquals(true, literal.values.head)
		assertEquals(Types.Bool, literal.nodeType)
		assertEquals("True", literal.code)
		assertEquals(0, literal.height)
		assertEquals(1, literal.terms)
		assertTrue(literal.children.isEmpty)
	}

	@Test def intToStringNode(): Unit =
	{
		val node: IntToString = new IntToString(new IntLiteral(83, 1))
		assertEquals(1, node.values.length)
		assertEquals("83", node.values.head)
		assertEquals(Types.String, node.nodeType)
		assertEquals("str(83)", node.code)
		assertEquals(1, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def stringToIntNode(): Unit =
	{
		val node: StringToInt = new StringToInt(new StringLiteral("83", 1))
		assertEquals(1, node.values.length)
		assertEquals(83, node.values.head)
		assertEquals(Types.Int, node.nodeType)
		assertEquals("int(\"83\")", node.code)
		assertEquals(1, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def stringLengthNode(): Unit =
	{
		val node: StringLength = new StringLength(new StringLiteral("83", 1))
		assertEquals(1, node.values.length)
		assertEquals(2, node.values.head)
		assertEquals(Types.Int, node.nodeType)
		assertEquals("len(\"83\")", node.code)
		assertEquals(1, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def stringLowerNode(): Unit =
	{
		var node: StringLower = new StringLower(new StringLiteral("aBC", 1))
		assertEquals(1, node.values.length)
		assertEquals("abc", node.values.head)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"aBC\".lower()", node.code)
		assertEquals(1, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)

		node = new StringLower(new StringConcat(
			new StringLiteral("aBC", 1),
			new StringLiteral("deF", 1)))
		assertEquals(1, node.values.length)
		assertEquals("abcdef", node.values.head)
		assertEquals(Types.String, node.nodeType)
		assertEquals("(\"aBC\" + \"deF\").lower()", node.code)
		assertEquals(2, node.height)
		assertEquals(4, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def maxNode(): Unit =
	{
		val node: Max = new Max(new IntListNode {
			override val values: List[Iterable[Int]] = List(-1123 :: 2 :: 1 :: Nil)
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false
		})
		assertEquals(1, node.values.length)
		assertEquals(2, node.values.head)
		assertEquals(Types.Int, node.nodeType)
		assertEquals("max([-1123, 2, 1])", node.code)
		assertEquals(2, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	@Test def minNode(): Unit =
	{
		val node: Min = new Min(new IntListNode {
			override val values: List[Iterable[Int]] = List(-1123 :: 2 :: 1 :: Nil)
			override val code: String = "[-1123, 2, 1]"
			override val height: Int = 1
			override val terms: Int = 1
			override val children: Iterable[ASTNode] = Nil

			override def includes(varName: String): Boolean = false
		})
		assertEquals(1, node.values.length)
		assertEquals(-1123, node.values.head)
		assertEquals(Types.Int, node.nodeType)
		assertEquals("min([-1123, 2, 1])", node.code)
		assertEquals(2, node.height)
		assertEquals(2, node.terms)
		assertEquals(node.children.size, 1)
	}

	// Binary Operations
	@Test def binarySubstringNode(): Unit =
	{
		val str: StringNode = new StringLiteral("abc", 1)

		var node: BinarySubstring = new BinarySubstring(str, new IntLiteral(0,1))
		assertEquals(1, node.values.length)
		assertEquals("a", node.values.head)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[0]", node.code)
		assertEquals(1, node.height)
		assertEquals(3, node.terms)
		assertEquals(node.children.size, 2)

		node = new BinarySubstring(str, new IntLiteral(1,1))
		assertEquals(1, node.values.length)
		assertEquals("b", node.values.head)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[1]", node.code)
		assertEquals(1, node.height)
		assertEquals(3, node.terms)
		assertEquals(node.children.size, 2)

		node = new BinarySubstring(str, new IntLiteral(2,1))
		assertEquals(1, node.values.length)
		assertEquals("c", node.values.head)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[2]", node.code)
		assertEquals(1, node.height)
		assertEquals(3, node.terms)
		assertEquals(node.children.size, 2)

		node = new BinarySubstring(str, new IntLiteral(3,1))
		assertEquals(0, node.values.length)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[3]", node.code)
		assertEquals(1, node.height)
		assertEquals(3, node.terms)
		assertEquals(node.children.size, 2)
	}

	// Ternary Operations
	@Test def ternarySubstringNode(): Unit =
	{
		val str: StringNode = new StringLiteral("abc", 1)
		var node: TernarySubstring = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(3,1))
		assertEquals(1, node.values.length)
		assertEquals("abc", node.values.head)
		assertEquals(Types.String, node.nodeType)
		assertEquals("\"abc\"[0:3]", node.code)
		assertEquals(1, node.height)
		assertEquals(4, node.terms)
		assertEquals(node.children.size, 3)

		// [-4, -3] -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(-4,1),
			new IntLiteral(-3,1))
		assertEquals("", node.values.head)

		// [-4, -2] -> "a"
		node = new TernarySubstring(
			str,
			new IntLiteral(-4,1),
			new IntLiteral(-2,1))
		assertEquals("a", node.values.head)

		// [-4, -1] -> "ab"
		node = new TernarySubstring(
			str,
			new IntLiteral(-4,1),
			new IntLiteral(-1,1))
		assertEquals("ab", node.values.head)

		// [-4, 0]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(-4,1),
			new IntLiteral(0,1))
		assertEquals("", node.values.head)

		// [-4, 1]  -> "a"
		node = new TernarySubstring(
			str,
			new IntLiteral(-4,1),
			new IntLiteral(1,1))
		assertEquals("a", node.values.head)

		// [-4, 2]  -> "ab"
		node = new TernarySubstring(
			str,
			new IntLiteral(-4,1),
			new IntLiteral(2,1))
		assertEquals("ab", node.values.head)

		// [-4, 3]  -> "abc"
		node = new TernarySubstring(
			str,
			new IntLiteral(-4,1),
			new IntLiteral(3,1))
		assertEquals("abc", node.values.head)

		// [-4, 4]  -> "abc"
		node = new TernarySubstring(
			str,
			new IntLiteral(-4,1),
			new IntLiteral(4,1))
		assertEquals("abc", node.values.head)

		// [0, -4]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(-4,1))
		assertEquals("", node.values.head)

		// [0, -3]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(-3,1))
		assertEquals("", node.values.head)

		// [0, -2]  -> "a"
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(-2,1))
		assertEquals("a", node.values.head)

		// [0, -1]  -> "ab"
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(-1, 1))
		assertEquals("ab", node.values.head)

		// [0, 0]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(0, 1))
		assertEquals("", node.values.head)

		// [0, 1]  -> "a"
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(1, 1))
		assertEquals("a", node.values.head)

		// [0, 2]  -> "ab"
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(2, 1))
		assertEquals("ab", node.values.head)

		// [0, 3]  -> "abc"
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(3, 1))
		assertEquals("abc", node.values.head)

		// [0, 4]  -> "abc"
		node = new TernarySubstring(
			str,
			new IntLiteral(0,1),
			new IntLiteral(4, 1))
		assertEquals("abc", node.values.head)

		// [1, -4]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(-4, 1))
		assertEquals("", node.values.head)

		// [1, -3]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(-3, 1))
		assertEquals("", node.values.head)

		// [1, -2]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(-2, 1))
		assertEquals("", node.values.head)

		// [1, -1]  -> "b"
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(-1, 1))
		assertEquals("b", node.values.head)

		// [1, 0]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(0, 1))
		assertEquals("", node.values.head)

		// [1, 1]  -> ""
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(1, 1))
		assertEquals("", node.values.head)

		// [1, 2]  -> "b"
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(2, 1))
		assertEquals("b", node.values.head)

		// [1, 3]  -> "bc"
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(3, 1))
		assertEquals("bc", node.values.head)

		// [1, 4]  -> "bc"
		node = new TernarySubstring(
			str,
			new IntLiteral(1,1),
			new IntLiteral(4, 1))
		assertEquals("bc", node.values.head)


		// [3, -4]  -> ""
		// [3, -3]  -> ""
		// [3, -2]  -> ""
		// [3, -1]  -> ""
		// [3, 0]  -> ""
		// [3, 1]  -> ""
		// [3, 2]  -> ""
		// [3, 3]  -> ""
		// [3, 4]  -> ""
		for (i <- -3 to 4) {
			node = new TernarySubstring(
				str,
				new IntLiteral(3,1),
				new IntLiteral(i, 1))
			assertEquals("", node.values.head)
		}
	}

	// TODO Write the unit tests for other nodes
	// Ternary Operations
	@Test def stringConcatNode(): Unit = ()
	@Test def stringStepNode(): Unit = ()
	@Test def intAdditionNode(): Unit = ()
	@Test def intSubtractionNode(): Unit = ()
	@Test def intDivisionNode(): Unit = ()
	@Test def findNode(): Unit = ()
	@Test def containsNode(): Unit = ()
	@Test def stringReplaceNode(): Unit = ()

	// Quaternary Operations
	@Test def quaternarySubstringNode(): Unit = ()

	// List Operations
	@Test def stringSplitNode(): Unit = ()
	@Test def stringJoinNode(): Unit = ()
	@Test def stringStepListNode(): Unit = ()
	@Test def substringListNode(): Unit = ()
	@Test def stringToIntListNode(): Unit = ()
	@Test def sortedStringListNode(): Unit = ()
}