use crate::component::{generate_component_root, generate_component_root_ty};
use gosub_css3::matcher::property_definitions::{get_css_properties, get_css_values};
use syn::__private::ToTokens;

const PRINT_DETAILED_FAILURES: bool = true;

#[test]
fn test_component_root_comparison_properties() {
    let properties = get_css_properties();
    let mut total_tested = 0;
    let mut failures = Vec::new();

    for (prop_name, prop_def) in properties {
        if prop_def.syntax.components.is_empty() {
            continue;
        }

        total_tested += 1;

        // Generate using the original method
        let original_result = generate_component_root(&prop_def.syntax.components[0], &prop_name);

        // Generate using the type-based method
        let type_result = generate_component_root_ty(&prop_def.syntax.components[0], &prop_name);

        match (original_result, type_result) {
            (Some(orig), Some((main_css_type, additional_css_types))) => {
                // Convert CssType to comparable format
                let ty_converted_main = main_css_type.to_type();
                let ty_converted_additional: Vec<_> = additional_css_types
                    .iter()
                    .map(|css_type| css_type.to_type())
                    .collect();

                // Compare the main structures
                let orig_tokens = orig.0.to_token_stream().to_string();
                let ty_tokens = ty_converted_main.to_token_stream().to_string();

                if orig_tokens != ty_tokens {
                    failures.push(ComparisonFailure {
                        property_name: prop_name.clone(),
                        failure_type: FailureType::MainStructureDifference,
                        original_output: orig_tokens,
                        type_output: ty_tokens,
                        additional_details: None,
                    });

                    if PRINT_DETAILED_FAILURES {
                        println!(
                            "FAILURE in property '{}': Main structure difference",
                            prop_name
                        );
                        println!("Original  : {}", orig.0.to_token_stream());
                        println!("Type-based: {}", ty_converted_main.to_token_stream());
                        println!("---");
                    }
                }

                // Compare additional structures count
                if orig.1.len() != ty_converted_additional.len() {
                    failures.push(ComparisonFailure {
                        property_name: prop_name.clone(),
                        failure_type: FailureType::AdditionalStructuresCountMismatch,
                        original_output: format!("Count: {}", orig.1.len()),
                        type_output: format!("Count: {}", ty_converted_additional.len()),
                        additional_details: Some(format!(
                            "Original has {} additional structures, Type-based has {}",
                            orig.1.len(),
                            ty_converted_additional.len()
                        )),
                    });

                    if PRINT_DETAILED_FAILURES {
                        println!(
                            "FAILURE in property '{}': Additional structures count mismatch",
                            prop_name
                        );
                        println!(
                            "Original count: {}, Type-based count: {}",
                            orig.1.len(),
                            ty_converted_additional.len()
                        );
                    }
                }

                // Compare each additional structure
                for (i, (orig_additional, ty_additional)) in orig
                    .1
                    .iter()
                    .zip(ty_converted_additional.iter())
                    .enumerate()
                {
                    let orig_add_tokens = orig_additional.to_token_stream().to_string();
                    let ty_add_tokens = ty_additional.to_token_stream().to_string();

                    if orig_add_tokens != ty_add_tokens {
                        failures.push(ComparisonFailure {
                            property_name: prop_name.clone(),
                            failure_type: FailureType::AdditionalStructureDifference(i),
                            original_output: orig_add_tokens,
                            type_output: ty_add_tokens,
                            additional_details: None,
                        });

                        if PRINT_DETAILED_FAILURES {
                            println!(
                                "FAILURE in property '{}': Additional structure {} difference",
                                prop_name, i
                            );
                            println!("Original  : {}", orig_additional.to_token_stream());
                            println!("Type-based: {}", ty_additional.to_token_stream());
                            println!("---");
                        }
                    }
                }
            }
            (None, Some(_)) => {
                failures.push(ComparisonFailure {
                    property_name: prop_name.clone(),
                    failure_type: FailureType::OriginalNoneTypeSuccess,
                    original_output: "None".to_string(),
                    type_output: "Some(...)".to_string(),
                    additional_details: None,
                });

                if PRINT_DETAILED_FAILURES {
                    println!("FAILURE in property '{}': Original returned None, Type-based returned Some", prop_name);
                }
            }
            (Some(_), None) => {
                failures.push(ComparisonFailure {
                    property_name: prop_name.clone(),
                    failure_type: FailureType::OriginalSuccessTypeNone,
                    original_output: "Some(...)".to_string(),
                    type_output: "None".to_string(),
                    additional_details: None,
                });

                if PRINT_DETAILED_FAILURES {
                    println!("FAILURE in property '{}': Original returned Some, Type-based returned None", prop_name);
                }
            }
            (None, None) => {
                // Both failed, this is expected behavior for some cases
            }
        }
    }

    println!("\n=== TEST SUMMARY ===");
    println!("Total properties tested: {}", total_tested);
    println!("Total failures: {}", failures.len());

    if !failures.is_empty() {
        println!("\nFailure breakdown:");
        let mut failure_counts = std::collections::HashMap::new();
        for failure in &failures {
            *failure_counts
                .entry(failure.failure_type.name())
                .or_insert(0) += 1;
        }

        for (failure_type, count) in failure_counts {
            println!("  {}: {}", failure_type, count);
        }

        if !PRINT_DETAILED_FAILURES {
            println!("\nSet PRINT_DETAILED_FAILURES = true to see detailed failure information");
        }

        // Print first few failures as examples
        println!("\nFirst 3 failures:");
        for failure in failures.iter().take(3) {
            println!("Property: {}", failure.property_name);
            println!("Type: {}", failure.failure_type.name());
            if let Some(details) = &failure.additional_details {
                println!("Details: {}", details);
            }
            println!("---");
        }
    }

    assert_eq!(
        failures.len(),
        0,
        "Found {} comparison failures",
        failures.len()
    );
}

#[test]
fn test_component_root_comparison_values() {
    let values = get_css_values();
    let mut total_tested = 0;
    let mut failures = Vec::new();

    for (val_name, val_def) in values {
        if val_def.syntax.components.is_empty() {
            continue;
        }

        total_tested += 1;

        // Generate using the original method
        let original_result = generate_component_root(&val_def.syntax.components[0], &val_name);

        // Generate using the type-based method
        let type_result = generate_component_root_ty(&val_def.syntax.components[0], &val_name);

        match (original_result, type_result) {
            (Some(orig), Some((main_css_type, additional_css_types))) => {
                // Convert CssType to comparable format
                let ty_converted_main = main_css_type.to_type();
                let ty_converted_additional: Vec<_> = additional_css_types
                    .iter()
                    .map(|css_type| css_type.to_type())
                    .collect();

                // Compare the main structures
                let orig_tokens = orig.0.to_token_stream().to_string();
                let ty_tokens = ty_converted_main.to_token_stream().to_string();

                if orig_tokens != ty_tokens {
                    failures.push(ComparisonFailure {
                        property_name: val_name.clone(),
                        failure_type: FailureType::MainStructureDifference,
                        original_output: orig_tokens,
                        type_output: ty_tokens,
                        additional_details: None,
                    });

                    if PRINT_DETAILED_FAILURES {
                        println!(
                            "FAILURE in property '{}': Main structure difference",
                            val_name
                        );
                        println!("Original  : {}", orig.0.to_token_stream());
                        println!("Type-based: {}", ty_converted_main.to_token_stream());
                        println!("---");
                    }
                }

                // Compare additional structures count
                if orig.1.len() != ty_converted_additional.len() {
                    failures.push(ComparisonFailure {
                        property_name: val_name.clone(),
                        failure_type: FailureType::AdditionalStructuresCountMismatch,
                        original_output: format!("Count: {}", orig.1.len()),
                        type_output: format!("Count: {}", ty_converted_additional.len()),
                        additional_details: Some(format!(
                            "Original has {} additional structures, Type-based has {}",
                            orig.1.len(),
                            ty_converted_additional.len()
                        )),
                    });

                    if PRINT_DETAILED_FAILURES {
                        println!(
                            "FAILURE in property '{}': Additional structures count mismatch",
                            val_name
                        );
                        println!(
                            "Original count: {}, Type-based count: {}",
                            orig.1.len(),
                            ty_converted_additional.len()
                        );
                    }
                }

                // Compare each additional structure
                for (i, (orig_additional, ty_additional)) in orig
                    .1
                    .iter()
                    .zip(ty_converted_additional.iter())
                    .enumerate()
                {
                    let orig_add_tokens = orig_additional.to_token_stream().to_string();
                    let ty_add_tokens = ty_additional.to_token_stream().to_string();

                    if orig_add_tokens != ty_add_tokens {
                        failures.push(ComparisonFailure {
                            property_name: val_name.clone(),
                            failure_type: FailureType::AdditionalStructureDifference(i),
                            original_output: orig_add_tokens,
                            type_output: ty_add_tokens,
                            additional_details: None,
                        });

                        if PRINT_DETAILED_FAILURES {
                            println!(
                                "FAILURE in property '{}': Additional structure {} difference",
                                val_name, i
                            );
                            println!("Original  : {}", orig_additional.to_token_stream());
                            println!("Type-based: {}", ty_additional.to_token_stream());
                            println!("---");
                        }
                    }
                }
            }
            (None, Some(_)) => {
                failures.push(ComparisonFailure {
                    property_name: val_name.clone(),
                    failure_type: FailureType::OriginalNoneTypeSuccess,
                    original_output: "None".to_string(),
                    type_output: "Some(...)".to_string(),
                    additional_details: None,
                });

                if PRINT_DETAILED_FAILURES {
                    println!("FAILURE in property '{}': Original returned None, Type-based returned Some", val_name);
                }
            }
            (Some(_), None) => {
                failures.push(ComparisonFailure {
                    property_name: val_name.clone(),
                    failure_type: FailureType::OriginalSuccessTypeNone,
                    original_output: "Some(...)".to_string(),
                    type_output: "None".to_string(),
                    additional_details: None,
                });

                if PRINT_DETAILED_FAILURES {
                    println!("FAILURE in property '{}': Original returned Some, Type-based returned None", val_name);
                }
            }
            (None, None) => {
                // Both failed, this is expected behavior for some cases
            }
        }
    }

    println!("\n=== TEST SUMMARY ===");
    println!("Total properties tested: {}", total_tested);
    println!("Total failures: {}", failures.len());

    if !failures.is_empty() {
        println!("\nFailure breakdown:");
        let mut failure_counts = std::collections::HashMap::new();
        for failure in &failures {
            *failure_counts
                .entry(failure.failure_type.name())
                .or_insert(0) += 1;
        }

        for (failure_type, count) in failure_counts {
            println!("  {}: {}", failure_type, count);
        }

        if !PRINT_DETAILED_FAILURES {
            println!("\nSet PRINT_DETAILED_FAILURES = true to see detailed failure information");
        }

        // Print first few failures as examples
        println!("\nFirst 3 failures:");
        for failure in failures.iter().take(3) {
            println!("Property: {}", failure.property_name);
            println!("Type: {}", failure.failure_type.name());
            if let Some(details) = &failure.additional_details {
                println!("Details: {}", details);
            }
            println!("---");
        }
    }

    assert_eq!(
        failures.len(),
        0,
        "Found {} comparison failures",
        failures.len()
    );
}

#[test]
fn test_component_root_comparison_sample() {
    // Test with a specific known case for debugging
    let properties = get_css_properties();

    // Find a specific property to test (e.g., "color" if it exists)
    if let Some((prop_name, prop_def)) = properties.iter().find(|(name, _)| *name == "color") {
        if !prop_def.syntax.components.is_empty() {
            let original_result =
                generate_component_root(&prop_def.syntax.components[0], prop_name);
            let type_result = generate_component_root_ty(&prop_def.syntax.components[0], prop_name);

            if PRINT_DETAILED_FAILURES {
                println!("Testing property: {}", prop_name);

                match (&original_result, &type_result) {
                    (Some(orig), Some((main_css_type, additional_css_types))) => {
                        let ty_converted_main = main_css_type.to_type();
                        println!("Original result:");
                        println!("{}", orig.0.to_token_stream());
                        println!("\nType-based result:");
                        println!("{}", ty_converted_main.to_token_stream());
                        println!("\nOriginal additional count: {}", orig.1.len());
                        println!(
                            "Type-based additional count: {}",
                            additional_css_types.len()
                        );
                    }
                    (None, None) => println!("Both methods returned None"),
                    (Some(_), None) => println!("Original: Some, Type-based: None"),
                    (None, Some(_)) => println!("Original: None, Type-based: Some"),
                }
            }

            // Add assertions based on expected behavior
            match (original_result, type_result) {
                (Some(orig), Some((main_css_type, _additional_css_types))) => {
                    let ty_converted_main = main_css_type.to_type();
                    // Add specific assertions here
                    assert_eq!(
                        orig.0.to_token_stream().to_string(),
                        ty_converted_main.to_token_stream().to_string(),
                        "Main structures should match for property '{}'",
                        prop_name
                    );
                }
                _ => {
                    // Handle cases where one or both return None
                }
            }
        }
    }
}

#[test]
fn grid_auto_flow() {
    let properties = get_css_properties();

    let prop = "border-clip-left";

    let grid_auto_flow = properties.get(prop).unwrap();

    let original_result =
        generate_component_root(&grid_auto_flow.syntax.components[0], prop);
    let type_result =
        generate_component_root_ty(&grid_auto_flow.syntax.components[0], prop);

    if PRINT_DETAILED_FAILURES {
        println!("Testing property: {prop}");

        match (&original_result, &type_result) {
            (Some(orig), Some((main_css_type, additional_css_types))) => {
                dbg!(&main_css_type);
                let ty_converted_main = main_css_type.to_type();
                println!("Original result:");
                println!("{}", orig.0.to_token_stream());
                println!("\nType-based result:");
                println!("{}", ty_converted_main.to_token_stream());
                println!("\nOriginal additional count: {}", orig.1.len());
                println!(
                    "Type-based additional count: {}",
                    additional_css_types.len()
                );

                if orig.0.to_token_stream().to_string()
                    != ty_converted_main.to_token_stream().to_string()
                {
                    panic!("Mismatch in main structure for 'grid-auto-flow'");
                }
            }
            (None, None) => println!("Both methods returned None"),
            (Some(_), None) => println!("Original: Some, Type-based: None"),
            (None, Some(_)) => println!("Original: None, Type-based: Some"),
        }
    }
}

#[derive(Debug)]
struct ComparisonFailure {
    property_name: String,
    failure_type: FailureType,
    original_output: String,
    type_output: String,
    additional_details: Option<String>,
}

#[derive(Debug)]
enum FailureType {
    MainStructureDifference,
    AdditionalStructuresCountMismatch,
    AdditionalStructureDifference(usize),
    OriginalNoneTypeSuccess,
    OriginalSuccessTypeNone,
}

impl FailureType {
    fn name(&self) -> &'static str {
        match self {
            FailureType::MainStructureDifference => "Main Structure Difference",
            FailureType::AdditionalStructuresCountMismatch => {
                "Additional Structures Count Mismatch"
            }
            FailureType::AdditionalStructureDifference(_) => "Additional Structure Difference",
            FailureType::OriginalNoneTypeSuccess => "Original None, Type Success",
            FailureType::OriginalSuccessTypeNone => "Original Success, Type None",
        }
    }
}
