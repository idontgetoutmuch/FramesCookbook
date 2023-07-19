% Cartography in Haskell
% Dominic Steinitz
% 7th May 2018

Introduction
============


> {-# LANGUAGE DataKinds #-}
> {-# LANGUAGE TemplateHaskell #-}
> {-# LANGUAGE ConstraintKinds #-}
> {-# LANGUAGE FlexibleContexts #-}
> {-# LANGUAGE FlexibleInstances #-}
> {-# LANGUAGE GADTs #-}
> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# LANGUAGE RankNTypes #-}
> {-# LANGUAGE ScopedTypeVariables #-}
> {-# LANGUAGE StandaloneKindSignatures #-}
> {-# LANGUAGE TypeApplications #-}
> {-# LANGUAGE TypeFamilies #-}
> {-# LANGUAGE TypeOperators #-}
> {-# LANGUAGE UndecidableInstances #-}
> {-# LANGUAGE PolyKinds #-}
> {-# LANGUAGE AllowAmbiguousTypes #-}
> {-# LANGUAGE OverloadedStrings #-}

> {-# OPTIONS_GHC -Wall -Wno-type-defaults #-}

> import Data.Singletons.TH (genSingletons)
> import Data.Vinyl.CoRec hiding (Field)
> import Data.Vinyl.Core hiding (rpureConstrained, rpureConstraints)
> import Data.Proxy
> import Data.Vinyl.Functor
> import Data.Vinyl.TypeLevel
> import Data.Vinyl.Lens
> import Data.Vinyl.Derived
> import Data.Vinyl.Recursive (rpureConstrained)
> import Lens.Micro
> import GHC.TypeLits hiding (Nat)

> import           Frames
> import           Frames.CSV
> import           Frames.Melt (RDeleteAll, ElemOf)
> import           Data.Vinyl.TypeLevel as V
> import qualified Data.Vinyl as V


Frames Cookbook
===============

Renaming a Column
-----------------

> transform :: forall rs as bs . (as ⊆ rs, RDeleteAll as rs ⊆ rs)
>              => (Record as -> Record bs) -> Record rs -> Record (RDeleteAll as rs ++ bs)
> transform f xs = rcast @(RDeleteAll as rs) xs `rappend` f (rcast xs)

> retypeColumn :: forall x y rs . ( V.KnownField x
>                                 , V.KnownField y
>                                 , V.Snd x ~ V.Snd y
>                                 , ElemOf rs x
>                                 , RDelete x rs ⊆ rs)
>   => Record rs -> Record (RDelete x rs V.++ '[y])
> retypeColumn = transform @rs @'[x] @'[y] (\r -> (rgetField @x r &: V.RNil))

> type PreBenchmarks = '["Variety" :-> Text, "Mean" :-> Double]
>
> readRec' :: (RMap rs, ReadRec rs) => [Text] -> Rec ElField rs
> readRec' = rmap (either (error "Cannot parse") id . getCompose) . readRec

> rec1 :: Record PreBenchmarks
> rec1 = readRec' ["Male", "1.80"]

    [ghci]
    :t rec1

> rec2 :: Record '[ '("Variety", Text), '("NewMean", Double)]
> rec2 = retypeColumn @'("Mean", Double) @'("NewMean", Double) rec1

    [ghci]
    :t rec2

To Be Explained
---------------

> eitherToMaybe :: (RMap rs, ReadRec rs) => [Text] -> Rec (Compose Maybe ElField) rs
> eitherToMaybe = rmap (either (const (Compose Nothing)) (Compose . Just) . getCompose) . readRec

Concepts
========

fields and their types are encoded at the type level in an extensible way.
This means that, contrary to static types that Haskell, Aeson etc use, we
need some type level collection (type level list), type level functions to
check what elements we have in this list, and higher order types (kinds) to
define the type of the elements in this list.

type level list of unique identifiers
[Name, Age, Height, Weight] :: '['Fields]

        vvvvv

type level function
Interpretor :: 'Fields -> *

        vvvvv

type level list of concrete data types
[String, Int, Double, Double] :: '[*]

Field Universe "Fields":
This defines a data kind that basically identifies the fields of the record
It is later mapped to a concrete type * (e.g. Int or String) through the
Interpretor type family (basically a type level function).

> data Fields = Name | Age | Height deriving (Show)

> genSingletons [''Fields]

Interpretor: This is a type family, think type level function, from the
Field Universe "Fields" to a concrete type. This Interpretor is also the
first type level argument to build a record with the Rec type.

> type family Interpretor (f :: Fields) :: * where
>   Interpretor Name = String
>   Interpretor Age = Int
>   Interpretor Height = Double

InterpretorType: Type families are not first class in Haskell. This means
that they can't be passed as an argument to another type (e.g. such as we
can pass functions to other functions). We work around this limitation
with a concrete type that wraps our type family so that we can
pass it as an argument to a Rec type. Note that the following type can take
an argument that is not of kind * but of kind 'Fields.

> newtype InterpretedType (f :: Fields) = Itp {_unItp :: Interpretor f}

> instance forall (f :: Fields). Show (Interpretor f) => Show (InterpretedType f) where
>   show (Itp x) = show x

We can now build an operator that connects an identifier of kind 'Fields
with a value of the associated concrete type as computed by our type
level function Interpretor.

> (=::) :: sing f -> Interpretor f -> InterpretedType f
> (=::) _ = Itp

We can now construct an example Vinyl record that associates the type level
field identifiers of kind 'Fields with a concrete type via the
InterpretedType that is just a box for types derived from the labels
via the type family Interpretor

> exampleRecord1 :: Rec InterpretedType '[ 'Age, 'Name]
> exampleRecord1 = SAge =:: 20 :& SName =:: "Alice" :& RNil

> exampleRecord2 :: Rec InterpretedType '[ 'Height, 'Name]
> exampleRecord2 = SHeight =:: 1.85 :& SName =:: "Bob" :& RNil

> exampleRecord3 :: Rec InterpretedType '[ 'Name]
> exampleRecord3 = SName =:: "Bob" :& RNil

simple rappend is just acting on the type level list. Therefore
we get duplicate entries. The ++ operator is also a type family.

> composedOverlappingRecord :: Rec InterpretedType ('[ 'Age, 'Name] ++ '[ 'Height, 'Name])
> composedOverlappingRecord = rappend exampleRecord1 exampleRecord2

Question: The previous examples with cons :& require all fields to be defined
in order. How can I define a record out of order just by type?

FUNCTOR GAMES
=============

exampleRecord with different Interpretor Identity

> identityRecord :: Rec Identity '[Int]
> identityRecord = Identity 3 :& RNil

> elFieldRecord :: Rec ElField '[ '("height", Double), '("age", Int), '("name", String)]
> elFieldRecord = Field @"height" 20.0 :& Field @"age" 20 :& Field @"name" "Charlie" :& RNil

We can build new functors by composing (basically nesting) others with the
Compose type. It basically wraps the nested Functors in a new concrete type.

> type MaybeElField = Compose Maybe ElField

Functors can be converted into each other. Be careful because this needs
to be valid for _any_ x that is stored in them. In other words, you can
basically only move _everything_ from one container to the next without
changing any value since you don't know what the value will be. You can also
not selectively delete anything. One example where this works is wrapping a
value with Maybe. Maybe is just putting all existing values into a new
container Just and adds another value Nothing. This can be done for any type
whatsoever, which means that we can build a natural transform from any
container to Maybe by simply wrapping it in it.

> nat1 :: forall x. ElField x -> MaybeElField x
> nat1 (Field x) = Compose $ Just $ Field x

We can map such a natural transform over a record as follows:

> mappedOverRecord1 :: Rec MaybeElField '[ '("height", Double), '("age", Int), '("name", String)]
> mappedOverRecord1 = rmap nat1 elFieldRecord

LENSES
======

Micro Lens defines the following type synonym for a general getter/setter
type Getting r s a = (a -> Const r a) -> s -> Const r s

s stands here for the type of the *s*tructure,
r for the type of the *r*eturn type,
and *a* for the type within s that we want to modify.

This is what rlens gives us

rlens :: [...] => (f r -> g (f r)) -> record f rs -> g (record f rs)

We can compare this to Getting as follows

s = record f rs : this is the type of the complete structure
a = f r : the type of an element within a larger structure. This could, for
    example, be f=ElField r='("height", Double),
r = The return type which is determined by the particular operation we
are doing.

For example, the operator "^. :: s -> Getting a s a -> a" is used to
simply return the value that belongs to a certain type:

height :: ElField '("height", Double)
height = elFieldRecord ^. rlens @'("height", Double)

How can I assign a value of a target field?

> elFieldRecord2 :: Rec ElField '[ '("height", Double), '("age", Int), '("name", String)]
> elFieldRecord2 = elFieldRecord & rlens @'("height", Double) .~ Field 5.0

How can I map something over a target field?

> elFieldRecord3 :: Rec ElField '[ '("height", Double), '("age", Int), '("name", String)]
> elFieldRecord3 = elFieldRecord & rlens @'("height", Double) %~ (\(Field x) -> Field (2*x))

How can I change a field to a different type?

> elFieldRecord5 :: Rec ElField '[ '("new height", Integer), '("age", Int), '("name", String)]
> elFieldRecord5 = elFieldRecord & rlens' @'("height", Double) %~ (\(Field x) -> Field @"new height" 19)

with a helper function

> rename :: forall l' l v. KnownSymbol l' => ElField '(l, v) -> ElField '(l', v)
> rename (Field x) = Field @l' @v x

> elFieldRecord4 :: Rec ElField '[ '("new name", Double), '("age", Int), '("name", String)]
> elFieldRecord4 = elFieldRecord & rlens' @'("height", Double) %~ rename @"new name"

How can I get all ElField's of a certain type?

> rgetType :: forall a rs rs' ss.
>         (ss ~ GetAllTypeFields a rs, RSubset ss rs (RImage ss rs)) =>
>         Proxy a -> Rec ElField rs -> Rec ElField ss
> rgetType p = rcast

> elFieldRecord6 :: Rec ElField '[ '("new name", Double)]
> elFieldRecord6 = rgetType (Proxy @Double) elFieldRecord4

How can I get all field _values_ that are of type x?
How can I get all labels for fields that are of type x?
How can I get all fields that fullfil a certain constraint?
How can I apply a class method to all fields?
How can I fold over a list of labels?
How can I fold over all records with a certain type?
How can I get all fields from a list of types?

MODIFYING RECORDS AT TYPE LEVEL

We will now go through a few functions to Create Read Delete Update
records information. Since we defined record fields on the _type level_,
this involves lots of type level functions, aka type families. Consider
this type:

> type MyFields = '[ '("name", String), '("age", Int)]

How can we obtain the name of the second column from it?
There are two ways, one at the type level, for example useful to define
another record with this specific field, and another at run time, for
example, to print the label of the second field. Let's dive into type
level first, although it's a bit more difficult.
Here is a first function that gives me a type level number of kind NAT
that is the length of the record fields:

> type LengthMyType = RLength MyFields

I can check this in ghci (:kind! evaluates type families/synonyms and prints
them) with:

    [ghci]
    :kind! LengthMyType

The latter indeed means that it's the successor (S) of the successor (S) of
Zero. In other words the number 2.

This type can also be converted to a standard Int runtime value

> l :: Int
> l = natToInt @LengthMyType

l correctly prints 2 in ghci

    [ghci]
    l

Next experiment is to recover the index of a certain field:

> type MyAgeIndex = RIndex '("age", Int) MyFields

This prints

    [ghci]
    :kind! MyAgeIndex

In other words, it recovers the index 1 of this particular field.

We can now try to delete this field from the record

> type MyFields2 = RDelete '("age", Int) MyFields

    [ghci]
    :kind! MyFields2

unsurprisingly this worked as well. One more:

> type MyFields3 = MyFields ++ '[ '("height", Double), '("weight", Double)]

    [ghci]
    :kind! MyFields3

> type MyAgeIndex2 = MapTyCon Maybe MyFields3

> type AppliedField = ApplyToField Maybe '("height", Double)

get the type of a particular field

> type FieldTypeHeight = FieldType "height" MyFields3

Questions:

- How can we recover the index of an ElField only based on the field label?
  Use case: delete field with a specific label without remembering the whole
  type. This can be circumvented with a type synonym but that's a bit less
  attractive.

Here is an example how a field can be recovered simply from the label

> type RecoveredField = '("height", FieldType "height" MyFields3)

And here is how we can build a smarter type family that extracts the index
of a field just based on the label

> type family RFieldIndex (k :: s) (rs :: [(s, k2)]) :: Nat where
>        RFieldIndex k rs = RIndex '(k, FieldType k rs) rs

    [ghci]
    :kind! RFieldIndex "height" MyFields3

- How can we recover all fields with a certain type?

> type family GetAllTypeFields (a :: *) (rs :: [(Symbol, *)]) :: [(Symbol, *)] where
>         GetAllTypeFields a ( '(x, a) ': rs') = '(x, a) : GetAllTypeFields a rs'
>         GetAllTypeFields a ( '(x, b) ': rs') = GetAllTypeFields a rs'
>         GetAllTypeFields a '[] = '[]

    [ghci]
    :kind! GetAllTypeFields Double MyFields3

How can we delete a subset?
Use case: remove a set of columns

> type family RemoveSubset (ss :: [k]) (rs :: [k]) :: [k] where
>         RemoveSubset (s ': ss') rs = RemoveSubset ss' (RDelete s rs)
>         RemoveSubset '[] rs = rs
>         RemoveSubset ss '[] = '[]

- How can we recover all fields with a certain constraint?

> -- type family GetAllConstraintFields (c :: * -> Constraint) (rs :: [(Symbol, *)]) :: [(Symbol, *)] where
> --         GetAllConstraintFields c ( '(x, a) ': rs') = '(x, a) : GetAllConstraintFields c rs'
> --         GetAllConstraintFields c ( '(x, b) ': rs') = GetAllConstraintFields c rs'
> --         GetAllConstraintFields c '[] = '[]

PLAYING WITH CONSTRAINTS
========================

Natural transforms are very limited because we can't perform actions that
depend on the type of the value. We thus still don't know how to mass-change
values in a record because it could hold values of any type and GHC somehow
needs to understand what we can and cannot do with them. The way this works
is basically via constraints that specify that types have certain
interfaces, typeclasses in Haskell. If all types in a record have a Show
interface, we can, for example, print them. The following shows an
artificial type class Doubler that doubles fields, including String.

> class Doubler a where
>   double :: a -> a

> instance Doubler String where
>   double a = a <> a

> instance Doubler Int where
>   double a = 2 * a

> -- instance Doubler Double where
> --   double a = 2 * a

> instance forall (a :: Fields). Doubler (Interpretor a) => Doubler (InterpretedType a) where
>   double (Itp x) = Itp $ double x

> instance forall a k (s :: Symbol).
>     (k ~ '(s, a), Doubler a) =>
>     Doubler (ElField k) where
>   double (Field x) = Field $ double x

The following is a constraint record. To understand what is going on, it's
helpful to look at the type of the Dict data constructor: Dict :: c a => a -> Dict c a
Whatever gets wrapped in Dict, fullfills the constraint that is given in its
first type parameter. If we compose this now with our original InterpretedType, any
type has to fullfill this constraint as well. Otherwise we wouldn't be able to compose.
GHC understands this and knows explicitly that each type fullfills the constraint.
Here is the composed Functor.

> type DoublerTypes = Dict Doubler :. InterpretedType

And here is how it looks like if we convert a record to one with explicit
constraints

> constraintRecord1 :: Rec DoublerTypes '[ 'Age, 'Name]
> constraintRecord1 = reifyConstraint @Doubler exampleRecord1

The following fails

> -- constraintRecord2 = reifyConstraint @Doubler exampleRecord2
> -- with error:
> -- • No instance for (Doubler Double)
> --    arising from a use of ‘reifyConstraint’
> -- because we didn't define a Doubler instance for Double

With this new, constraint record, we can use our typeclasses in rmap
which is what we wanted in the first place.

> doubled :: Rec InterpretedType '[ 'Age, 'Name]
> doubled = rmap (\(Compose (Dict x)) -> double x) constraintRecord1

rpure serves to create a record filled with a default. It has type
(forall a. f a) -> Rec f a, which means that we need a constructor that
works for any (!) type, and even any kind. This is a strong restriction
and it essentially limits us to the use of things like Proxy:

> exampleRecPure1 :: Rec Proxy '[ '("age", String), '("height", Double)]
> exampleRecPure1 = rpure Proxy

this prints

    [ghci]
    exampleRecPure1

here is another option

> exampleRecPure2 :: Rec MaybeElField '[ '("name", String), '("age", Int), '("height", Double)]
> exampleRecPure2 = rpure $ Compose Nothing

I can change values in this record now

> exampleRecPure3 = rput (Compose $ Just $ Field @"name" @String "Joe") exampleRecPure2

    [ghci]
    exampleRecPure3

rpure is thus some kind of equivalent to pure outside of vinyl. Similar to
pure that generates a single value list, we here generate a default record.

more useful is often to derive this default value from a typeclass method.
However, this can't be applied to all types, of course but only those who
implement the desired typeclass interface. We can express this through a
constraint in Haskell. Vinyl provides the function rpureConstrained for
this. Look at this fun example:

> class (KnownField a, Monoid (Snd a)) => Helper a
> instance (KnownField a, Monoid (Snd a)) => Helper a

> exampleRecPure4 :: Rec ElField '[ '("list", [Double]), '("string", String) ]
> exampleRecPure4 = rpureConstrained (Proxy :: Proxy Helper) f
>     where
>         f :: Helper a => ElField a
>         f = Field mempty

CoRecords
=========

Where Records are similar to Product types in Haskell, able to store any
_combination_ of values (think AND), CoRecords are similar to standard Sum types and
can store any single value but not several at once (think OR). The Co comes
from the Categorical notion that a Product is the most "efficient" type that
any component can be extracted from, whereas the Sum is the most "efficient"
type that any component can be injected in—in this sense they are opposite
each other.

Vinyl comes with an extensible CoRec type that is quite similar to a Rec.

> exampleCoRec1 :: CoRec Identity '[ Int, String, Double]
> exampleCoRec1 = CoRec $ Identity @Int 3

> exampleCoRec2 :: CoRec InterpretedType '[ Name, Age, Height]
> exampleCoRec2 = CoRec $ Itp @Name "Alice"

> exampleCoRec3 :: CoRec ElField '[ '("Name", String), '("Age", Int), '("Height", Double)]
> exampleCoRec3 = CoRec $ Field @"Name" @String "Alice"

alternatively one can use the corec smart constructor which helps
with type inference when using ElFields.

> exampleCoRec4 :: CoRec ElField '[ '("Name", String), '("Age", Int), '("Height", Double), '("Weight", Double)]
> exampleCoRec4 = corec (Field @"Name" "Bob")

> a :: Maybe (ElField '("Name", String))
> a = asA' exampleCoRec4

> exampleCoRec5a :: CoRec ElField '[ '("Name", String), '("Height1", Double), '( "Height2", Double)]
> exampleCoRec5a = corec $ Field @"Height1" 1.85

> exampleCoRec5b :: CoRec ElField '[ '("Name", String), '("Height1", Double), '( "Height2", Double)]
> exampleCoRec5b = corec $ Field @"Height2" 1.90

> exampleCoRec5c :: CoRec ElField '[ '("Height1", Double), '( "Height2", Double)]
> exampleCoRec5c = corec $ Field @"Height2" 1.90

The following is the approach taken by Frames to use a constraint on CoRec
fields

> class ConvertF a where
>         convertF :: ElField a -> Maybe Double

> instance forall s. KnownSymbol s => ConvertF '(s, Double) where
>         convertF (Field x) = Just x

> converter :: forall ts.
>         RPureConstrained ConvertF ts =>
>         CoRec ElField ts -> Maybe Double
> converter = onCoRec @ConvertF convertF

> converted1 = converter exampleCoRec5c

 The following fails on purpose on compile time:

> -- converted2 = converter exampleCoRec5b
> --
> -- The error is
> -- • No instance for (ConvertF '("Name", String))
> --     arising from a use of ‘converter’

Need to understand RMap and rmap before tackling this. Somehow this
relates to morphing one Interpretor into another and it's unclear to me
how this should work

> -- combinedOverlappingRecord :: Rec InterpretedType '[ 'Name]
> -- combinedOverlappingRecord = rcombine (<>) id id exampleRecord3 exampleRecord3

The CoRec type
==============

Lenses
======

Elfields
========

