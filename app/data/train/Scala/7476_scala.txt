package com.peterpotts.sample

import org.scalatest.{Matchers, WordSpec}

class SampleUsageTest extends WordSpec with Matchers {
  "A sample usage" should {
    "use sample tuple 5" in {
      object SamplePerson extends MapSample(SampleTuple5(
        SampleUUID,
        SampleString(3 to 8).map("Mr. " + _),
        SampleInt(50).filter(_ > 18),
        SampleOption(SampleString),
        SampleSeq(SampleString, 0 to 10)), Person.tupled)

      SamplePerson.next()
      SampleSeq(SamplePerson.distinctBy(_.name)).next()
    }

    "use sample identity" in {
      object SamplePerson extends SampleIdentity(
        for {
          id <- SampleUUID
          name <- SampleString(3 to 8).map("Mr. " + _)
          age <- SampleInt(50) if age > 18
          email <- SampleOption(SampleString)
          friends <- SampleSeq(SampleString, 0 to 10)
        } yield Person(id, name, age, email, friends))

      SamplePerson.next()
      SampleSeq(SamplePerson.distinctBy(_.name)).next()
    }

    "use variable like an object" in {
      val SamplePerson = for {
        id <- SampleUUID
        name <- SampleString(3 to 8).map("Mr. " + _)
        age <- SampleInt(50) if age > 18
        email <- SampleOption(SampleString)
        friends <- SampleSeq(SampleString, 0 to 10)
      } yield Person(id, name, age, email, friends)

      SamplePerson.next()
      SampleSeq(SamplePerson.distinctBy(_.name)).next()
    }
  }
}
