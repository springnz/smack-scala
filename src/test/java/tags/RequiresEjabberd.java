package tags;
import java.lang.annotation.*;
import java.lang.annotation.RetentionPolicy;
import java.lang.annotation.ElementType;

@org.scalatest.TagAnnotation
@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.METHOD, ElementType.TYPE})
public @interface RequiresEjabberd {
}
