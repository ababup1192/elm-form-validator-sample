package gradle.cucumber;

import cucumber.api.PendingException;
import cucumber.api.java.en.Given;
import cucumber.api.java.en.Then;
import cucumber.api.java.en.When;
import org.openqa.selenium.By;
import org.openqa.selenium.WebDriver;
import org.openqa.selenium.WebElement;
import org.openqa.selenium.chrome.ChromeDriver;
import org.openqa.selenium.chrome.ChromeOptions;

import static junit.framework.TestCase.assertTrue;


public class BasicStepDefs {

    WebDriver driver = null;

    @Given("^Form Validationを開き$")
    public void Form_Validationを開き() throws Throwable {
        System.setProperty("webdriver.chrome.driver", "Driver/chromedriver");
        driver = new ChromeDriver();
        driver.get("https://ababup1192.github.io/elm-form-validator-sample/");
    }

    @When("^最初のフォームに(\\d+)を入力し$")
    public void 最初のフォームに_を入力し(int x) throws Throwable {
        WebElement input = driver.findElement(By.cssSelector("input[type=\"number\"]"));
        input.clear();
        input.sendKeys(String.valueOf(x));
    }

    @When("^次のフォームに、`http://foo.com` と入力し$")
    public void 次のフォームに_http_foo_com_と入力し() throws Throwable {
        WebElement input = driver.findElement(By.cssSelector("input[type=\"text\"]"));
        input.clear();
        input.sendKeys("http://foo.com");
    }

    @When("^submitボタンを押すと$")
    public void submitボタンを押すと() throws Throwable {
        WebElement button = driver.findElement(By.cssSelector("input[type=\"button\"]"));
        button.click();
    }

    @Then("^Submittedと表示される$")
    public void Submittedと表示される() throws Throwable {
        Thread.sleep(1000);
        WebElement resElement = driver.findElement(By.tagName("h1"));
        assertTrue(resElement.getText().contains("Submitted"));
        driver.quit();
    }
}
