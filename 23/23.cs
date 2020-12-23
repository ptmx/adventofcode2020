using System;
using System.Collections.Generic;
using System.Linq;

namespace AdventOfCode2020 {
  public class P23 {
    private readonly static List<int> INPUT = new List<int>{ 3, 9, 8, 2, 5, 4, 7, 1, 6 };
    private readonly static int PART_1_TURNS = 100;
    private readonly static int PART_2_TURNS = 10000000;

    public static void Main() {
      Part1(INPUT);
      Part2(INPUT);
    }

    private static void Part1(List<int> cups) {
      LinkedList<int> finalCups = PlayGame(cups, PART_1_TURNS);
      var joinedCups = String.Join("", finalCups);
      var oneIndex = joinedCups.IndexOf("1");
      Console.WriteLine(joinedCups.Substring(oneIndex + 1) + joinedCups.Substring(0, oneIndex));
    }

    private static void Part2(List<int> cups) {
      List<int> part2Cups = new List<int>(cups);
      for (int i = 10; i <= 1000000; i++) part2Cups.Add(i);

      var finalCups = PlayGame(part2Cups, PART_2_TURNS);
      var oneNode = finalCups.Find(1);
      var nextCupNode = oneNode.GetNextNode();
      var nextNextCupNode = nextCupNode.GetNextNode();
      Console.WriteLine((long)nextCupNode.Value * (long)nextNextCupNode.Value);
    }

    private static LinkedList<int> PlayGame(List<int> startingCups, int turns) {
      var cups = new LinkedList<int>{};
      var cupIndex = new Dictionary<int, LinkedListNode<int>>();
      foreach (var cup in startingCups) {
        var cupNode = new LinkedListNode<int>(cup);
        cupIndex[cup] = cupNode;
        cups.AddLast(cupNode);
      }

      var totalCups = cups.Count;
      var currentCup = startingCups[0];

      foreach (var _ in Enumerable.Range(0, turns)) {
        var currentCupNode = cupIndex[currentCup];

        var pickedUpCupNodes = new List<LinkedListNode<int>>{};
        var nodeToPickUp = currentCupNode;
        foreach (var __ in Enumerable.Range(0, 3)) {
          nodeToPickUp = nodeToPickUp.GetNextNode();
          pickedUpCupNodes.Add(nodeToPickUp);
        }
        var pickedUpCups = pickedUpCupNodes.Select(node => node.Value).ToList();

        var destinationCup = currentCup == 1 ? totalCups : currentCup - 1;
        while (pickedUpCups.Contains(destinationCup)) {
          destinationCup = destinationCup == 1 ? totalCups : destinationCup - 1;
        }

        var destinationCupNode = cupIndex[destinationCup];
        foreach (var pickedUpNode in pickedUpCupNodes) {
          cups.Remove(pickedUpNode);
        }

        var destinationNode = destinationCupNode;
        foreach (var cup in pickedUpCups) {
          var nextNode = new LinkedListNode<int>(cup);
          cupIndex[cup] = nextNode;
          cups.AddAfter(destinationNode, nextNode);
          destinationNode = nextNode;
        }

        var nextCupNode = currentCupNode.GetNextNode();
        currentCup = nextCupNode.Value;
      }

      return cups;
    }
  }

  static class CircularLinkedList {
    public static LinkedListNode<T> GetNextNode<T>(this LinkedListNode<T> node) {
      return node.Next == null
        ? node.List.First
        : node.Next;
    }
  }
}
