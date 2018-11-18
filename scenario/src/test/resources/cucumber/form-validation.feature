Feature: Form Validation
  Validationをするサンプル

  Scenario: 正しい値を入力することにより、Submit出来る
    Given Form Validationを開き
    When 最初のフォームに15を入力し
    And  次のフォームに、`http://foo.com` と入力し
    And submitボタンを押すと
    Then Submittedと表示される